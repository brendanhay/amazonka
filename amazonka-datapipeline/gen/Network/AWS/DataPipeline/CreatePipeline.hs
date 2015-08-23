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
-- Module      : Network.AWS.DataPipeline.CreatePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty pipeline. Use PutPipelineDefinition to populate the
-- pipeline.
--
-- /See:/ <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_CreatePipeline.html AWS API Reference> for CreatePipeline.
module Network.AWS.DataPipeline.CreatePipeline
    (
    -- * Creating a Request
      createPipeline
    , CreatePipeline
    -- * Request Lenses
    , cpDescription
    , cpTags
    , cpName
    , cpUniqueId

    -- * Destructuring the Response
    , createPipelineResponse
    , CreatePipelineResponse
    -- * Response Lenses
    , cprsStatus
    , cprsPipelineId
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.DataPipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CreatePipeline.
--
-- /See:/ 'createPipeline' smart constructor.
data CreatePipeline = CreatePipeline'
    { _cpDescription :: !(Maybe Text)
    , _cpTags        :: !(Maybe [Tag])
    , _cpName        :: !Text
    , _cpUniqueId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreatePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpDescription'
--
-- * 'cpTags'
--
-- * 'cpName'
--
-- * 'cpUniqueId'
createPipeline
    :: Text -- ^ 'cpName'
    -> Text -- ^ 'cpUniqueId'
    -> CreatePipeline
createPipeline pName_ pUniqueId_ =
    CreatePipeline'
    { _cpDescription = Nothing
    , _cpTags = Nothing
    , _cpName = pName_
    , _cpUniqueId = pUniqueId_
    }

-- | The description for the pipeline.
cpDescription :: Lens' CreatePipeline (Maybe Text)
cpDescription = lens _cpDescription (\ s a -> s{_cpDescription = a});

-- | A list of tags to associate with the pipeline at creation. Tags let you
-- control access to pipelines. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines>
-- in the /AWS Data Pipeline Developer Guide/.
cpTags :: Lens' CreatePipeline [Tag]
cpTags = lens _cpTags (\ s a -> s{_cpTags = a}) . _Default . _Coerce;

-- | The name for the pipeline. You can use the same name for multiple
-- pipelines associated with your AWS account, because AWS Data Pipeline
-- assigns each pipeline a unique pipeline identifier.
cpName :: Lens' CreatePipeline Text
cpName = lens _cpName (\ s a -> s{_cpName = a});

-- | A unique identifier. This identifier is not the same as the pipeline
-- identifier assigned by AWS Data Pipeline. You are responsible for
-- defining the format and ensuring the uniqueness of this identifier. You
-- use this parameter to ensure idempotency during repeated calls to
-- 'CreatePipeline'. For example, if the first call to 'CreatePipeline'
-- does not succeed, you can pass in the same unique identifier and
-- pipeline name combination on a subsequent call to 'CreatePipeline'.
-- 'CreatePipeline' ensures that if a pipeline already exists with the same
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
                 CreatePipelineResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "pipelineId"))

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
              (catMaybes
                 [("description" .=) <$> _cpDescription,
                  ("tags" .=) <$> _cpTags, Just ("name" .= _cpName),
                  Just ("uniqueId" .= _cpUniqueId)])

instance ToPath CreatePipeline where
        toPath = const "/"

instance ToQuery CreatePipeline where
        toQuery = const mempty

-- | Contains the output of CreatePipeline.
--
-- /See:/ 'createPipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
    { _cprsStatus     :: !Int
    , _cprsPipelineId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreatePipelineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsStatus'
--
-- * 'cprsPipelineId'
createPipelineResponse
    :: Int -- ^ 'cprsStatus'
    -> Text -- ^ 'cprsPipelineId'
    -> CreatePipelineResponse
createPipelineResponse pStatus_ pPipelineId_ =
    CreatePipelineResponse'
    { _cprsStatus = pStatus_
    , _cprsPipelineId = pPipelineId_
    }

-- | The response status code.
cprsStatus :: Lens' CreatePipelineResponse Int
cprsStatus = lens _cprsStatus (\ s a -> s{_cprsStatus = a});

-- | The ID that AWS Data Pipeline assigns the newly created pipeline. For
-- example, 'df-06372391ZG65EXAMPLE'.
cprsPipelineId :: Lens' CreatePipelineResponse Text
cprsPipelineId = lens _cprsPipelineId (\ s a -> s{_cprsPipelineId = a});
