{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EMR.AddJobFlowSteps
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | AddJobFlowSteps adds new steps to a running job flow. A maximum of 256
-- steps are allowed in each job flow.
--
-- If your job flow is long-running (such as a Hive data warehouse) or
-- complex, you may require more than 256 steps to process your data. You
-- can bypass the 256-step limitation in various ways, including using the
-- SSH shell to connect to the master node and submitting queries directly
-- to the software running on the master node, such as Hive and Hadoop. For
-- more information on how to do this, go to
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/AddMoreThan256Steps.html Add More than 256 Steps to a Job Flow>
-- in the /Amazon Elastic MapReduce Developer\'s Guide/.
--
-- A step specifies the location of a JAR file stored either on the master
-- node of the job flow or in Amazon S3. Each step is performed by the main
-- function of the main class of the JAR file. The main class can be
-- specified either in the manifest of the JAR or by using the MainFunction
-- parameter of the step.
--
-- Elastic MapReduce executes each step in the order listed. For a step to
-- be considered complete, the main function must exit with a zero exit
-- code and all Hadoop jobs started while the step was running must have
-- completed and run successfully.
--
-- You can only add steps to a job flow that is in one of the following
-- states: STARTING, BOOTSTRAPPING, RUNNING, or WAITING.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_AddJobFlowSteps.html>
module Network.AWS.EMR.AddJobFlowSteps
    (
    -- * Request
      AddJobFlowSteps
    -- ** Request constructor
    , addJobFlowSteps
    -- ** Request lenses
    , ajfsJobFlowId
    , ajfsSteps

    -- * Response
    , AddJobFlowStepsResponse
    -- ** Response constructor
    , addJobFlowStepsResponse
    -- ** Response lenses
    , ajfsrStepIds
    , ajfsrStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input argument to the AddJobFlowSteps operation.
--
-- /See:/ 'addJobFlowSteps' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ajfsJobFlowId'
--
-- * 'ajfsSteps'
data AddJobFlowSteps = AddJobFlowSteps'
    { _ajfsJobFlowId :: !Text
    , _ajfsSteps     :: ![StepConfig]
    } deriving (Eq,Read,Show)

-- | 'AddJobFlowSteps' smart constructor.
addJobFlowSteps :: Text -> AddJobFlowSteps
addJobFlowSteps pJobFlowId =
    AddJobFlowSteps'
    { _ajfsJobFlowId = pJobFlowId
    , _ajfsSteps = mempty
    }

-- | A string that uniquely identifies the job flow. This identifier is
-- returned by RunJobFlow and can also be obtained from ListClusters.
ajfsJobFlowId :: Lens' AddJobFlowSteps Text
ajfsJobFlowId = lens _ajfsJobFlowId (\ s a -> s{_ajfsJobFlowId = a});

-- | A list of StepConfig to be executed by the job flow.
ajfsSteps :: Lens' AddJobFlowSteps [StepConfig]
ajfsSteps = lens _ajfsSteps (\ s a -> s{_ajfsSteps = a});

instance AWSRequest AddJobFlowSteps where
        type Sv AddJobFlowSteps = EMR
        type Rs AddJobFlowSteps = AddJobFlowStepsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AddJobFlowStepsResponse' <$>
                   (x .?> "StepIds" .!@ mempty) <*> (pure s))

instance ToHeaders AddJobFlowSteps where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.AddJobFlowSteps" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddJobFlowSteps where
        toJSON AddJobFlowSteps'{..}
          = object
              ["JobFlowId" .= _ajfsJobFlowId,
               "Steps" .= _ajfsSteps]

instance ToPath AddJobFlowSteps where
        toPath = const "/"

instance ToQuery AddJobFlowSteps where
        toQuery = const mempty

-- | The output for the AddJobFlowSteps operation.
--
-- /See:/ 'addJobFlowStepsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ajfsrStepIds'
--
-- * 'ajfsrStatus'
data AddJobFlowStepsResponse = AddJobFlowStepsResponse'
    { _ajfsrStepIds :: !(Maybe [Text])
    , _ajfsrStatus  :: !Status
    } deriving (Eq,Show)

-- | 'AddJobFlowStepsResponse' smart constructor.
addJobFlowStepsResponse :: Status -> AddJobFlowStepsResponse
addJobFlowStepsResponse pStatus =
    AddJobFlowStepsResponse'
    { _ajfsrStepIds = Nothing
    , _ajfsrStatus = pStatus
    }

-- | The identifiers of the list of steps added to the job flow.
ajfsrStepIds :: Lens' AddJobFlowStepsResponse [Text]
ajfsrStepIds = lens _ajfsrStepIds (\ s a -> s{_ajfsrStepIds = a}) . _Default;

-- | FIXME: Undocumented member.
ajfsrStatus :: Lens' AddJobFlowStepsResponse Status
ajfsrStatus = lens _ajfsrStatus (\ s a -> s{_ajfsrStatus = a});
