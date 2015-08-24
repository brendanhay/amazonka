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
-- Module      : Network.AWS.ElastiCache.RemoveTagsFromResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /RemoveTagsFromResource/ action removes the tags identified by the
-- 'TagKeys' list from the named resource.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_RemoveTagsFromResource.html AWS API Reference> for RemoveTagsFromResource.
module Network.AWS.ElastiCache.RemoveTagsFromResource
    (
    -- * Creating a Request
      removeTagsFromResource
    , RemoveTagsFromResource
    -- * Request Lenses
    , rtfrResourceName
    , rtfrTagKeys

    -- * Destructuring the Response
    , tagListMessage
    , TagListMessage
    -- * Response Lenses
    , tlmTagList
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /RemoveTagsFromResource/ action.
--
-- /See:/ 'removeTagsFromResource' smart constructor.
data RemoveTagsFromResource = RemoveTagsFromResource'
    { _rtfrResourceName :: !Text
    , _rtfrTagKeys      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveTagsFromResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfrResourceName'
--
-- * 'rtfrTagKeys'
removeTagsFromResource
    :: Text -- ^ 'rtfrResourceName'
    -> RemoveTagsFromResource
removeTagsFromResource pResourceName_ =
    RemoveTagsFromResource'
    { _rtfrResourceName = pResourceName_
    , _rtfrTagKeys = mempty
    }

-- | The name of the ElastiCache resource from which you want the listed tags
-- removed, for example
-- 'arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster'.
rtfrResourceName :: Lens' RemoveTagsFromResource Text
rtfrResourceName = lens _rtfrResourceName (\ s a -> s{_rtfrResourceName = a});

-- | A list of 'TagKeys' identifying the tags you want removed from the named
-- resource. For example, 'TagKeys.member.1=Region' removes the cost
-- allocation tag with the key name 'Region' from the resource named by the
-- /ResourceName/ parameter.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\ s a -> s{_rtfrTagKeys = a}) . _Coerce;

instance AWSRequest RemoveTagsFromResource where
        type Rs RemoveTagsFromResource = TagListMessage
        request = postQuery elastiCache
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
