{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.RemoveTagsFromResource
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

-- | The /RemoveTagsFromResource/ action removes the tags identified by the
-- @TagKeys@ list from the named resource.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_RemoveTagsFromResource.html>
module Network.AWS.ElastiCache.RemoveTagsFromResource
    (
    -- * Request
      RemoveTagsFromResource
    -- ** Request constructor
    , removeTagsFromResource
    -- ** Request lenses
    , rtfrResourceName
    , rtfrTagKeys

    -- * Response
    , TagListMessage
    -- ** Response constructor
    , tagListMessage
    -- ** Response lenses
    , tlmTagList
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /RemoveTagsFromResource/ action.
--
-- /See:/ 'removeTagsFromResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfrResourceName'
--
-- * 'rtfrTagKeys'
data RemoveTagsFromResource = RemoveTagsFromResource'
    { _rtfrResourceName :: Text
    , _rtfrTagKeys      :: [Text]
    } deriving (Eq,Read,Show)

-- | 'RemoveTagsFromResource' smart constructor.
removeTagsFromResource :: Text -> RemoveTagsFromResource
removeTagsFromResource pResourceName =
    RemoveTagsFromResource'
    { _rtfrResourceName = pResourceName
    , _rtfrTagKeys = mempty
    }

-- | The name of the ElastiCache resource from which you want the listed tags
-- removed, for example
-- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@.
rtfrResourceName :: Lens' RemoveTagsFromResource Text
rtfrResourceName = lens _rtfrResourceName (\ s a -> s{_rtfrResourceName = a});

-- | A list of @TagKeys@ identifying the tags you want removed from the named
-- resource. For example, @TagKeys.member.1=Region@ removes the cost
-- allocation tag with the key name @Region@ from the resource named by the
-- /ResourceName/ parameter.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\ s a -> s{_rtfrTagKeys = a});

instance AWSRequest RemoveTagsFromResource where
        type Sv RemoveTagsFromResource = ElastiCache
        type Rs RemoveTagsFromResource = TagListMessage
        request = post
        response
          = receiveXMLWrapper "RemoveTagsFromResourceResult"
              (\ s h x -> parseXML x)

instance ToHeaders RemoveTagsFromResource where
        toHeaders = const mempty

instance ToPath RemoveTagsFromResource where
        toPath = const "/"

instance ToQuery RemoveTagsFromResource where
        toQuery RemoveTagsFromResource'{..}
          = mconcat
              ["Action" =:
                 ("RemoveTagsFromResource" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "ResourceName" =: _rtfrResourceName,
               "TagKeys" =: toQueryList "member" _rtfrTagKeys]
