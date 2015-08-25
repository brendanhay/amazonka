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
-- Module      : Network.AWS.DataPipeline.RemoveTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes existing tags from the specified pipeline.
--
-- /See:/ <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_RemoveTags.html AWS API Reference> for RemoveTags.
module Network.AWS.DataPipeline.RemoveTags
    (
    -- * Creating a Request
      removeTags
    , RemoveTags
    -- * Request Lenses
    , rtPipelineId
    , rtTagKeys

    -- * Destructuring the Response
    , removeTagsResponse
    , RemoveTagsResponse
    -- * Response Lenses
    , rtrsStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.DataPipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for RemoveTags.
--
-- /See:/ 'removeTags' smart constructor.
data RemoveTags = RemoveTags'
    { _rtPipelineId :: !Text
    , _rtTagKeys    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtPipelineId'
--
-- * 'rtTagKeys'
removeTags
    :: Text -- ^ 'rtPipelineId'
    -> RemoveTags
removeTags pPipelineId_ =
    RemoveTags'
    { _rtPipelineId = pPipelineId_
    , _rtTagKeys = mempty
    }

-- | The ID of the pipeline.
rtPipelineId :: Lens' RemoveTags Text
rtPipelineId = lens _rtPipelineId (\ s a -> s{_rtPipelineId = a});

-- | The keys of the tags to remove.
rtTagKeys :: Lens' RemoveTags [Text]
rtTagKeys = lens _rtTagKeys (\ s a -> s{_rtTagKeys = a}) . _Coerce;

instance AWSRequest RemoveTags where
        type Rs RemoveTags = RemoveTagsResponse
        request = postJSON dataPipeline
        response
          = receiveEmpty
              (\ s h x ->
                 RemoveTagsResponse' <$> (pure (fromEnum s)))

instance ToHeaders RemoveTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.RemoveTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveTags where
        toJSON RemoveTags'{..}
          = object
              (catMaybes
                 [Just ("pipelineId" .= _rtPipelineId),
                  Just ("tagKeys" .= _rtTagKeys)])

instance ToPath RemoveTags where
        toPath = const "/"

instance ToQuery RemoveTags where
        toQuery = const mempty

-- | Contains the output of RemoveTags.
--
-- /See:/ 'removeTagsResponse' smart constructor.
newtype RemoveTagsResponse = RemoveTagsResponse'
    { _rtrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrsStatus'
removeTagsResponse
    :: Int -- ^ 'rtrsStatus'
    -> RemoveTagsResponse
removeTagsResponse pStatus_ =
    RemoveTagsResponse'
    { _rtrsStatus = pStatus_
    }

-- | The response status code.
rtrsStatus :: Lens' RemoveTagsResponse Int
rtrsStatus = lens _rtrsStatus (\ s a -> s{_rtrsStatus = a});
