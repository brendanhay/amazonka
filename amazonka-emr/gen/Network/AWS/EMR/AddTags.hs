{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EMR.AddTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds tags to an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clusters to track your Amazon
-- EMR resource allocation costs. For more information, see
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources>.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_AddTags.html>
module Network.AWS.EMR.AddTags
    (
    -- * Request
      AddTags
    -- ** Request constructor
    , addTags
    -- ** Request lenses
    , atResourceId
    , atTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , addTagsResponse
    -- ** Response lenses
    , atrStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input identifies a cluster and a list of tags to attach.
--
-- /See:/ 'addTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atResourceId'
--
-- * 'atTags'
data AddTags = AddTags'
    { _atResourceId :: !Text
    , _atTags       :: ![Tag]
    } deriving (Eq,Read,Show)

-- | 'AddTags' smart constructor.
addTags :: Text -> AddTags
addTags pResourceId =
    AddTags'
    { _atResourceId = pResourceId
    , _atTags = mempty
    }

-- | The Amazon EMR resource identifier to which tags will be added. This
-- value must be a cluster identifier.
atResourceId :: Lens' AddTags Text
atResourceId = lens _atResourceId (\ s a -> s{_atResourceId = a});

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances. Tags are user-defined key\/value pairs that consist of a
-- required key string with a maximum of 128 characters, and an optional
-- value string with a maximum of 256 characters.
atTags :: Lens' AddTags [Tag]
atTags = lens _atTags (\ s a -> s{_atTags = a});

instance AWSRequest AddTags where
        type Sv AddTags = EMR
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
                    ("ElasticMapReduce.AddTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTags where
        toJSON AddTags'{..}
          = object
              ["ResourceId" .= _atResourceId, "Tags" .= _atTags]

instance ToPath AddTags where
        toPath = const "/"

instance ToQuery AddTags where
        toQuery = const mempty

-- | This output indicates the result of adding tags to a resource.
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
