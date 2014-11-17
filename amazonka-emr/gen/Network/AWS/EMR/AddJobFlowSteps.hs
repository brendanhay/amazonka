{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.AddJobFlowSteps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AddJobFlowSteps adds new steps to a running job flow. A maximum of 256
-- steps are allowed in each job flow. If your job flow is long-running (such
-- as a Hive data warehouse) or complex, you may require more than 256 steps
-- to process your data. You can bypass the 256-step limitation in various
-- ways, including using the SSH shell to connect to the master node and
-- submitting queries directly to the software running on the master node,
-- such as Hive and Hadoop. For more information on how to do this, go to Add
-- More than 256 Steps to a Job Flow in the Amazon Elastic MapReduce
-- Developer's Guide. A step specifies the location of a JAR file stored
-- either on the master node of the job flow or in Amazon S3. Each step is
-- performed by the main function of the main class of the JAR file. The main
-- class can be specified either in the manifest of the JAR or by using the
-- MainFunction parameter of the step. Elastic MapReduce executes each step in
-- the order listed. For a step to be considered complete, the main function
-- must exit with a zero exit code and all Hadoop jobs started while the step
-- was running must have completed and run successfully. You can only add
-- steps to a job flow that is in one of the following states: STARTING,
-- BOOTSTRAPPING, RUNNING, or WAITING.
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
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

data AddJobFlowSteps = AddJobFlowSteps
    { _ajfsJobFlowId :: Text
    , _ajfsSteps     :: [StepConfig]
    } deriving (Eq, Show, Generic)

-- | 'AddJobFlowSteps' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ajfsJobFlowId' @::@ 'Text'
--
-- * 'ajfsSteps' @::@ ['StepConfig']
--
addJobFlowSteps :: Text -- ^ 'ajfsJobFlowId'
                -> AddJobFlowSteps
addJobFlowSteps p1 = AddJobFlowSteps
    { _ajfsJobFlowId = p1
    , _ajfsSteps     = mempty
    }

-- | A string that uniquely identifies the job flow. This identifier is
-- returned by RunJobFlow and can also be obtained from ListClusters.
ajfsJobFlowId :: Lens' AddJobFlowSteps Text
ajfsJobFlowId = lens _ajfsJobFlowId (\s a -> s { _ajfsJobFlowId = a })

-- | A list of StepConfig to be executed by the job flow.
ajfsSteps :: Lens' AddJobFlowSteps [StepConfig]
ajfsSteps = lens _ajfsSteps (\s a -> s { _ajfsSteps = a })

newtype AddJobFlowStepsResponse = AddJobFlowStepsResponse
    { _ajfsrStepIds :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList AddJobFlowStepsResponse where
    type Item AddJobFlowStepsResponse = Text

    fromList = AddJobFlowStepsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ajfsrStepIds

-- | 'AddJobFlowStepsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ajfsrStepIds' @::@ ['Text']
--
addJobFlowStepsResponse :: AddJobFlowStepsResponse
addJobFlowStepsResponse = AddJobFlowStepsResponse
    { _ajfsrStepIds = mempty
    }

-- | The identifiers of the list of steps added to the job flow.
ajfsrStepIds :: Lens' AddJobFlowStepsResponse [Text]
ajfsrStepIds = lens _ajfsrStepIds (\s a -> s { _ajfsrStepIds = a })

instance ToPath AddJobFlowSteps where
    toPath = const "/"

instance ToQuery AddJobFlowSteps where
    toQuery = const mempty

instance ToHeaders AddJobFlowSteps

instance ToJSON AddJobFlowSteps where
    toJSON AddJobFlowSteps{..} = object
        [ "JobFlowId" .= _ajfsJobFlowId
        , "Steps"     .= _ajfsSteps
        ]

instance AWSRequest AddJobFlowSteps where
    type Sv AddJobFlowSteps = EMR
    type Rs AddJobFlowSteps = AddJobFlowStepsResponse

    request  = post "AddJobFlowSteps"
    response = jsonResponse

instance FromJSON AddJobFlowStepsResponse where
    parseJSON = withObject "AddJobFlowStepsResponse" $ \o -> AddJobFlowStepsResponse
        <$> o .: "StepIds"
