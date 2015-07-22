{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.CreatePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty pipeline. Use PutPipelineDefinition to populate the
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
    , cprqDescription
    , cprqTags
    , cprqName
    , cprqUniqueId

    -- * Response
    , CreatePipelineResponse
    -- ** Response constructor
    , createPipelineResponse
    -- ** Response lenses
    , cprsStatus
    , cprsPipelineId
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CreatePipeline.
--
-- /See:/ 'createPipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprqDescription'
--
-- * 'cprqTags'
--
-- * 'cprqName'
--
-- * 'cprqUniqueId'
data CreatePipeline = CreatePipeline'
    { _cprqDescription :: !(Maybe Text)
    , _cprqTags        :: !(Maybe [Tag])
    , _cprqName        :: !Text
    , _cprqUniqueId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePipeline' smart constructor.
createPipeline :: Text -> Text -> CreatePipeline
createPipeline pName pUniqueId =
    CreatePipeline'
    { _cprqDescription = Nothing
    , _cprqTags = Nothing
    , _cprqName = pName
    , _cprqUniqueId = pUniqueId
    }

-- | The description for the pipeline.
cprqDescription :: Lens' CreatePipeline (Maybe Text)
cprqDescription = lens _cprqDescription (\ s a -> s{_cprqDescription = a});

-- | A list of tags to associate with the pipeline at creation. Tags let you
-- control access to pipelines. For more information, see
-- <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines>
-- in the /AWS Data Pipeline Developer Guide/.
cprqTags :: Lens' CreatePipeline [Tag]
cprqTags = lens _cprqTags (\ s a -> s{_cprqTags = a}) . _Default;

-- | The name for the pipeline. You can use the same name for multiple
-- pipelines associated with your AWS account, because AWS Data Pipeline
-- assigns each pipeline a unique pipeline identifier.
cprqName :: Lens' CreatePipeline Text
cprqName = lens _cprqName (\ s a -> s{_cprqName = a});

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
cprqUniqueId :: Lens' CreatePipeline Text
cprqUniqueId = lens _cprqUniqueId (\ s a -> s{_cprqUniqueId = a});

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
              ["description" .= _cprqDescription,
               "tags" .= _cprqTags, "name" .= _cprqName,
               "uniqueId" .= _cprqUniqueId]

instance ToPath CreatePipeline where
        toPath = const "/"

instance ToQuery CreatePipeline where
        toQuery = const mempty

-- | Contains the output of CreatePipeline.
--
-- /See:/ 'createPipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprsStatus'
--
-- * 'cprsPipelineId'
data CreatePipelineResponse = CreatePipelineResponse'
    { _cprsStatus     :: !Int
    , _cprsPipelineId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePipelineResponse' smart constructor.
createPipelineResponse :: Int -> Text -> CreatePipelineResponse
createPipelineResponse pStatus pPipelineId =
    CreatePipelineResponse'
    { _cprsStatus = pStatus
    , _cprsPipelineId = pPipelineId
    }

-- | FIXME: Undocumented member.
cprsStatus :: Lens' CreatePipelineResponse Int
cprsStatus = lens _cprsStatus (\ s a -> s{_cprsStatus = a});

-- | The ID that AWS Data Pipeline assigns the newly created pipeline. For
-- example, @df-06372391ZG65EXAMPLE@.
cprsPipelineId :: Lens' CreatePipelineResponse Text
cprsPipelineId = lens _cprsPipelineId (\ s a -> s{_cprsPipelineId = a});
