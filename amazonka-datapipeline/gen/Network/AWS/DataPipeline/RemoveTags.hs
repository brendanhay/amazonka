{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.RemoveTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes existing tags from the specified pipeline.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_RemoveTags.html>
module Network.AWS.DataPipeline.RemoveTags
    (
    -- * Request
      RemoveTags
    -- ** Request constructor
    , removeTags
    -- ** Request lenses
    , rtPipelineId
    , rtTagKeys

    -- * Response
    , RemoveTagsResponse
    -- ** Response constructor
    , removeTagsResponse
    -- ** Response lenses
    , rtrStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for RemoveTags.
--
-- /See:/ 'removeTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtPipelineId'
--
-- * 'rtTagKeys'
data RemoveTags = RemoveTags'
    { _rtPipelineId :: !Text
    , _rtTagKeys    :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTags' smart constructor.
removeTags :: Text -> RemoveTags
removeTags pPipelineId =
    RemoveTags'
    { _rtPipelineId = pPipelineId
    , _rtTagKeys = mempty
    }

-- | The ID of the pipeline.
rtPipelineId :: Lens' RemoveTags Text
rtPipelineId = lens _rtPipelineId (\ s a -> s{_rtPipelineId = a});

-- | The keys of the tags to remove.
rtTagKeys :: Lens' RemoveTags [Text]
rtTagKeys = lens _rtTagKeys (\ s a -> s{_rtTagKeys = a});

instance AWSRequest RemoveTags where
        type Sv RemoveTags = DataPipeline
        type Rs RemoveTags = RemoveTagsResponse
        request = postJSON
        response
          = receiveJSON
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
              ["pipelineId" .= _rtPipelineId,
               "tagKeys" .= _rtTagKeys]

instance ToPath RemoveTags where
        toPath = const "/"

instance ToQuery RemoveTags where
        toQuery = const mempty

-- | Contains the output of RemoveTags.
--
-- /See:/ 'removeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrStatus'
newtype RemoveTagsResponse = RemoveTagsResponse'
    { _rtrStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsResponse' smart constructor.
removeTagsResponse :: Int -> RemoveTagsResponse
removeTagsResponse pStatus =
    RemoveTagsResponse'
    { _rtrStatus = pStatus
    }

-- | FIXME: Undocumented member.
rtrStatus :: Lens' RemoveTagsResponse Int
rtrStatus = lens _rtrStatus (\ s a -> s{_rtrStatus = a});
