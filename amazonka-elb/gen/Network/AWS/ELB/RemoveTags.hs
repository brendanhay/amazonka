{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.RemoveTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified load balancer.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_RemoveTags.html>
module Network.AWS.ELB.RemoveTags
    (
    -- * Request
      RemoveTags
    -- ** Request constructor
    , removeTags
    -- ** Request lenses
    , rtLoadBalancerNames
    , rtTags

    -- * Response
    , RemoveTagsResponse
    -- ** Response constructor
    , removeTagsResponse
    -- ** Response lenses
    , rtrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'removeTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtLoadBalancerNames'
--
-- * 'rtTags'
data RemoveTags = RemoveTags'
    { _rtLoadBalancerNames :: ![Text]
    , _rtTags              :: !(List1 TagKeyOnly)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTags' smart constructor.
removeTags :: NonEmpty TagKeyOnly -> RemoveTags
removeTags pTags_ =
    RemoveTags'
    { _rtLoadBalancerNames = mempty
    , _rtTags = _List1 # pTags_
    }

-- | The name of the load balancer. You can specify a maximum of one load
-- balancer name.
rtLoadBalancerNames :: Lens' RemoveTags [Text]
rtLoadBalancerNames = lens _rtLoadBalancerNames (\ s a -> s{_rtLoadBalancerNames = a});

-- | The list of tag keys to remove.
rtTags :: Lens' RemoveTags (NonEmpty TagKeyOnly)
rtTags = lens _rtTags (\ s a -> s{_rtTags = a}) . _List1;

instance AWSRequest RemoveTags where
        type Sv RemoveTags = ELB
        type Rs RemoveTags = RemoveTagsResponse
        request = post "RemoveTags"
        response
          = receiveXMLWrapper "RemoveTagsResult"
              (\ s h x ->
                 RemoveTagsResponse' <$> (pure (fromEnum s)))

instance ToHeaders RemoveTags where
        toHeaders = const mempty

instance ToPath RemoveTags where
        toPath = const "/"

instance ToQuery RemoveTags where
        toQuery RemoveTags'{..}
          = mconcat
              ["Action" =: ("RemoveTags" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerNames" =:
                 toQueryList "member" _rtLoadBalancerNames,
               "Tags" =: toQueryList "member" _rtTags]

-- | /See:/ 'removeTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtrsStatus'
newtype RemoveTagsResponse = RemoveTagsResponse'
    { _rtrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsResponse' smart constructor.
removeTagsResponse :: Int -> RemoveTagsResponse
removeTagsResponse pStatus_ =
    RemoveTagsResponse'
    { _rtrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rtrsStatus :: Lens' RemoveTagsResponse Int
rtrsStatus = lens _rtrsStatus (\ s a -> s{_rtrsStatus = a});
