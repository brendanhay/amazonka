{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DataPipeline.AddTags
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

-- | Adds or modifies tags for the specified pipeline.
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
    , atrStatus
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
    } deriving (Eq,Read,Show)

-- | 'AddTags' smart constructor.
addTags :: Text -> AddTags
addTags pPipelineId =
    AddTags'
    { _atPipelineId = pPipelineId
    , _atTags = mempty
    }

-- | The ID of the pipeline.
atPipelineId :: Lens' AddTags Text
atPipelineId = lens _atPipelineId (\ s a -> s{_atPipelineId = a});

-- | The tags to add, as key\/value pairs.
atTags :: Lens' AddTags [Tag]
atTags = lens _atTags (\ s a -> s{_atTags = a});

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
-- * 'atrStatus'
newtype AddTagsResponse = AddTagsResponse'
    { _atrStatus :: Int
    } deriving (Eq,Read,Show)

-- | 'AddTagsResponse' smart constructor.
addTagsResponse :: Int -> AddTagsResponse
addTagsResponse pStatus =
    AddTagsResponse'
    { _atrStatus = pStatus
    }

-- | FIXME: Undocumented member.
atrStatus :: Lens' AddTagsResponse Int
atrStatus = lens _atrStatus (\ s a -> s{_atrStatus = a});
