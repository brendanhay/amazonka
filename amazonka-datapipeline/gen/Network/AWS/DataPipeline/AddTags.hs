{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.AddTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds or modifies tags for the specified pipeline.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_AddTags.html>
module Network.AWS.DataPipeline.AddTags
    (
    -- * Request
      AddTags
    -- ** Request constructor
    , addTags
    -- ** Request lenses
    , atPipelineId
    , atTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , addTagsResponse
    -- ** Response lenses
    , atrsStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for AddTags.
--
-- /See:/ 'addTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atPipelineId'
--
-- * 'atTags'
data AddTags = AddTags'
    { _atPipelineId :: !Text
    , _atTags       :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTags' smart constructor.
addTags :: Text -> AddTags
addTags pPipelineId_ =
    AddTags'
    { _atPipelineId = pPipelineId_
    , _atTags = mempty
    }

-- | The ID of the pipeline.
atPipelineId :: Lens' AddTags Text
atPipelineId = lens _atPipelineId (\ s a -> s{_atPipelineId = a});

-- | The tags to add, as key\/value pairs.
atTags :: Lens' AddTags [Tag]
atTags = lens _atTags (\ s a -> s{_atTags = a}) . _Coerce;

instance AWSRequest AddTags where
        type Sv AddTags = DataPipeline
        type Rs AddTags = AddTagsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x -> AddTagsResponse' <$> (pure (fromEnum s)))

instance ToHeaders AddTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.AddTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTags where
        toJSON AddTags'{..}
          = object
              ["pipelineId" .= _atPipelineId, "tags" .= _atTags]

instance ToPath AddTags where
        toPath = const "/"

instance ToQuery AddTags where
        toQuery = const mempty

-- | Contains the output of AddTags.
--
-- /See:/ 'addTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atrsStatus'
newtype AddTagsResponse = AddTagsResponse'
    { _atrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsResponse' smart constructor.
addTagsResponse :: Int -> AddTagsResponse
addTagsResponse pStatus_ =
    AddTagsResponse'
    { _atrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
atrsStatus :: Lens' AddTagsResponse Int
atrsStatus = lens _atrsStatus (\ s a -> s{_atrsStatus = a});
