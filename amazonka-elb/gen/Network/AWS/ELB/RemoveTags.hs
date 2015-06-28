{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.RemoveTags
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

-- | Removes one or more tags from the specified load balancer.
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
    , rtrStatus
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
    } deriving (Eq,Read,Show)

-- | 'RemoveTags' smart constructor.
removeTags :: NonEmpty TagKeyOnly -> RemoveTags
removeTags pTags =
    RemoveTags'
    { _rtLoadBalancerNames = mempty
    , _rtTags = _List1 # pTags
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
        request = post
        response
          = receiveXMLWrapper "RemoveTagsResult"
              (\ s h x -> RemoveTagsResponse' <$> (pure s))

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
-- * 'rtrStatus'
newtype RemoveTagsResponse = RemoveTagsResponse'
    { _rtrStatus :: Status
    } deriving (Eq,Read,Show)

-- | 'RemoveTagsResponse' smart constructor.
removeTagsResponse :: Status -> RemoveTagsResponse
removeTagsResponse pStatus =
    RemoveTagsResponse'
    { _rtrStatus = pStatus
    }

-- | FIXME: Undocumented member.
rtrStatus :: Lens' RemoveTagsResponse Status
rtrStatus = lens _rtrStatus (\ s a -> s{_rtrStatus = a});
