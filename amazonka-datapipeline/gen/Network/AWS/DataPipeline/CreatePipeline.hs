{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DataPipeline.CreatePipeline
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

-- | Creates a new, empty pipeline. Use PutPipelineDefinition to populate the
-- pipeline.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_CreatePipeline.html>
module Network.AWS.DataPipeline.CreatePipeline
    (
    -- * Request
      CreatePipeline
    -- ** Request constructor
    , createPipeline
    -- ** Request lenses
    , cpDescription
    , cpTags
    , cpName
    , cpUniqueId

    -- * Response
    , CreatePipelineResponse
    -- ** Response constructor
    , createPipelineResponse
    -- ** Response lenses
    , cprPipelineId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.DataPipeline.Types

-- | /See:/ 'createPipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpDescription'
--
-- * 'cpTags'
--
-- * 'cpName'
--
-- * 'cpUniqueId'
data CreatePipeline = CreatePipeline'{_cpDescription :: Maybe Text, _cpTags :: Maybe [Tag], _cpName :: Text, _cpUniqueId :: Text} deriving (Eq, Read, Show)

-- | 'CreatePipeline' smart constructor.
createPipeline :: Text -> Text -> CreatePipeline
createPipeline pName pUniqueId = CreatePipeline'{_cpDescription = Nothing, _cpTags = Nothing, _cpName = pName, _cpUniqueId = pUniqueId};

-- | The description for the pipeline.
cpDescription :: Lens' CreatePipeline (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a});

-- | A list of tags to associate with the pipeline at creation. Tags let you
-- control access to pipelines. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines>
-- in the /AWS Data Pipeline Developer Guide/.
cpTags :: Lens' CreatePipeline [Tag]
cpTags = lens _cpTags (\ s a -> s{_cpTags = a}) . _Default;

-- | The name for the pipeline. You can use the same name for multiple
-- pipelines associated with your AWS account, because AWS Data Pipeline
-- assigns each pipeline a unique pipeline identifier.
cpName :: Lens' CreatePipeline Text
cpName = lens _cpName (\ s a -> s{_cpName = a});

-- | A unique identifier. This identifier is not the same as the pipeline
-- identifier assigned by AWS Data Pipeline. You are responsible for
-- defining the format and ensuring the uniqueness of this identifier. You
-- use this parameter to ensure idempotency during repeated calls to
-- @CreatePipeline@. For example, if the first call to @CreatePipeline@
-- does not succeed, you can pass in the same unique identifier and
-- pipeline name combination on a subsequent call to @CreatePipeline@.
-- @CreatePipeline@ ensures that if a pipeline already exists with the same
-- name and unique identifier, a new pipeline is not created. Instead,
-- you\'ll receive the pipeline identifier from the previous attempt. The
-- uniqueness of the name and unique identifier combination is scoped to
-- the AWS account or IAM user credentials.
cpUniqueId :: Lens' CreatePipeline Text
cpUniqueId = lens _cpUniqueId (\ s a -> s{_cpUniqueId = a});

instance AWSRequest CreatePipeline where
        type Sv CreatePipeline = DataPipeline
        type Rs CreatePipeline = CreatePipelineResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreatePipelineResponse' <$> (x .:> "pipelineId"))

instance ToHeaders CreatePipeline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.CreatePipeline" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePipeline where
        toJSON CreatePipeline'{..}
          = object
              ["description" .= _cpDescription, "tags" .= _cpTags,
               "name" .= _cpName, "uniqueId" .= _cpUniqueId]

instance ToPath CreatePipeline where
        toPath = const "/"

instance ToQuery CreatePipeline where
        toQuery = const mempty

-- | /See:/ 'createPipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprPipelineId'
newtype CreatePipelineResponse = CreatePipelineResponse'{_cprPipelineId :: Text} deriving (Eq, Read, Show)

-- | 'CreatePipelineResponse' smart constructor.
createPipelineResponse :: Text -> CreatePipelineResponse
createPipelineResponse pPipelineId = CreatePipelineResponse'{_cprPipelineId = pPipelineId};

-- | The ID that AWS Data Pipeline assigns the newly created pipeline. For
-- example, @df-06372391ZG65EXAMPLE@.
cprPipelineId :: Lens' CreatePipelineResponse Text
cprPipelineId = lens _cprPipelineId (\ s a -> s{_cprPipelineId = a});
