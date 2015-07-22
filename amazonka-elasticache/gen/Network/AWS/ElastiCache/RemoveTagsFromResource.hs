{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.RemoveTagsFromResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /RemoveTagsFromResource/ action removes the tags identified by the
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
    , rtfrrqResourceName
    , rtfrrqTagKeys

    -- * Response
    , TagListMessage
    -- ** Response constructor
    , tagListMessage
    -- ** Response lenses
    , rtfrrsTagList
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
-- * 'rtfrrqResourceName'
--
-- * 'rtfrrqTagKeys'
data RemoveTagsFromResource = RemoveTagsFromResource'
    { _rtfrrqResourceName :: !Text
    , _rtfrrqTagKeys      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromResource' smart constructor.
removeTagsFromResource :: Text -> RemoveTagsFromResource
removeTagsFromResource pResourceName =
    RemoveTagsFromResource'
    { _rtfrrqResourceName = pResourceName
    , _rtfrrqTagKeys = mempty
    }

-- | The name of the ElastiCache resource from which you want the listed tags
-- removed, for example
-- @arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster@.
rtfrrqResourceName :: Lens' RemoveTagsFromResource Text
rtfrrqResourceName = lens _rtfrrqResourceName (\ s a -> s{_rtfrrqResourceName = a});

-- | A list of @TagKeys@ identifying the tags you want removed from the named
-- resource. For example, @TagKeys.member.1=Region@ removes the cost
-- allocation tag with the key name @Region@ from the resource named by the
-- /ResourceName/ parameter.
rtfrrqTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrrqTagKeys = lens _rtfrrqTagKeys (\ s a -> s{_rtfrrqTagKeys = a});

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
               "ResourceName" =: _rtfrrqResourceName,
               "TagKeys" =: toQueryList "member" _rtfrrqTagKeys]
