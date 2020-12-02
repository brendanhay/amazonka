{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddJobFlowSteps
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AddJobFlowSteps adds new steps to a running cluster. A maximum of 256 steps are allowed in each job flow.
--
--
-- If your cluster is long-running (such as a Hive data warehouse) or complex, you may require more than 256 steps to process your data. You can bypass the 256-step limitation in various ways, including using SSH to connect to the master node and submitting queries directly to the software running on the master node, such as Hive and Hadoop. For more information on how to do this, see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html Add More than 256 Steps to a Cluster> in the /Amazon EMR Management Guide/ .
--
-- A step specifies the location of a JAR file stored either on the master node of the cluster or in Amazon S3. Each step is performed by the main function of the main class of the JAR file. The main class can be specified either in the manifest of the JAR or by using the MainFunction parameter of the step.
--
-- Amazon EMR executes each step in the order listed. For a step to be considered complete, the main function must exit with a zero exit code and all Hadoop jobs started while the step was running must have completed and run successfully.
--
-- You can only add steps to a cluster that is in one of the following states: STARTING, BOOTSTRAPPING, RUNNING, or WAITING.
--
module Network.AWS.EMR.AddJobFlowSteps
    (
    -- * Creating a Request
      addJobFlowSteps
    , AddJobFlowSteps
    -- * Request Lenses
    , ajfsJobFlowId
    , ajfsSteps

    -- * Destructuring the Response
    , addJobFlowStepsResponse
    , AddJobFlowStepsResponse
    -- * Response Lenses
    , ajfsrsStepIds
    , ajfsrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input argument to the 'AddJobFlowSteps' operation.
--
--
--
-- /See:/ 'addJobFlowSteps' smart constructor.
data AddJobFlowSteps = AddJobFlowSteps'
  { _ajfsJobFlowId :: !Text
  , _ajfsSteps     :: ![StepConfig]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddJobFlowSteps' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajfsJobFlowId' - A string that uniquely identifies the job flow. This identifier is returned by 'RunJobFlow' and can also be obtained from 'ListClusters' .
--
-- * 'ajfsSteps' - A list of 'StepConfig' to be executed by the job flow.
addJobFlowSteps
    :: Text -- ^ 'ajfsJobFlowId'
    -> AddJobFlowSteps
addJobFlowSteps pJobFlowId_ =
  AddJobFlowSteps' {_ajfsJobFlowId = pJobFlowId_, _ajfsSteps = mempty}


-- | A string that uniquely identifies the job flow. This identifier is returned by 'RunJobFlow' and can also be obtained from 'ListClusters' .
ajfsJobFlowId :: Lens' AddJobFlowSteps Text
ajfsJobFlowId = lens _ajfsJobFlowId (\ s a -> s{_ajfsJobFlowId = a})

-- | A list of 'StepConfig' to be executed by the job flow.
ajfsSteps :: Lens' AddJobFlowSteps [StepConfig]
ajfsSteps = lens _ajfsSteps (\ s a -> s{_ajfsSteps = a}) . _Coerce

instance AWSRequest AddJobFlowSteps where
        type Rs AddJobFlowSteps = AddJobFlowStepsResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 AddJobFlowStepsResponse' <$>
                   (x .?> "StepIds" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable AddJobFlowSteps where

instance NFData AddJobFlowSteps where

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
              (catMaybes
                 [Just ("JobFlowId" .= _ajfsJobFlowId),
                  Just ("Steps" .= _ajfsSteps)])

instance ToPath AddJobFlowSteps where
        toPath = const "/"

instance ToQuery AddJobFlowSteps where
        toQuery = const mempty

-- | The output for the 'AddJobFlowSteps' operation.
--
--
--
-- /See:/ 'addJobFlowStepsResponse' smart constructor.
data AddJobFlowStepsResponse = AddJobFlowStepsResponse'
  { _ajfsrsStepIds        :: !(Maybe [Text])
  , _ajfsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddJobFlowStepsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajfsrsStepIds' - The identifiers of the list of steps added to the job flow.
--
-- * 'ajfsrsResponseStatus' - -- | The response status code.
addJobFlowStepsResponse
    :: Int -- ^ 'ajfsrsResponseStatus'
    -> AddJobFlowStepsResponse
addJobFlowStepsResponse pResponseStatus_ =
  AddJobFlowStepsResponse'
    {_ajfsrsStepIds = Nothing, _ajfsrsResponseStatus = pResponseStatus_}


-- | The identifiers of the list of steps added to the job flow.
ajfsrsStepIds :: Lens' AddJobFlowStepsResponse [Text]
ajfsrsStepIds = lens _ajfsrsStepIds (\ s a -> s{_ajfsrsStepIds = a}) . _Default . _Coerce

-- | -- | The response status code.
ajfsrsResponseStatus :: Lens' AddJobFlowStepsResponse Int
ajfsrsResponseStatus = lens _ajfsrsResponseStatus (\ s a -> s{_ajfsrsResponseStatus = a})

instance NFData AddJobFlowStepsResponse where
