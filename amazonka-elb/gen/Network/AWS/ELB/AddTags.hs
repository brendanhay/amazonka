{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.AddTags
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

-- | Adds the specified tags to the specified load balancer. Each load
-- balancer can have a maximum of 10 tags.
--
-- Each tag consists of a key and an optional value. If a tag with the same
-- key is already associated with the load balancer, @AddTags@ updates its
-- value.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/add-remove-tags.html Tag Your Load Balancer>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_AddTags.html>
module Network.AWS.ELB.AddTags
    (
    -- * Request
      AddTags
    -- ** Request constructor
    , addTags
    -- ** Request lenses
    , atLoadBalancerNames
    , atTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , addTagsResponse
    -- ** Response lenses
    , atrStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atLoadBalancerNames'
--
-- * 'atTags'
data AddTags = AddTags'
    { _atLoadBalancerNames :: ![Text]
    , _atTags              :: !(List1 Tag)
    } deriving (Eq,Read,Show)

-- | 'AddTags' smart constructor.
addTags :: NonEmpty Tag -> AddTags
addTags pTags =
    AddTags'
    { _atLoadBalancerNames = mempty
    , _atTags = _List1 # pTags
    }

-- | The name of the load balancer. You can specify one load balancer only.
atLoadBalancerNames :: Lens' AddTags [Text]
atLoadBalancerNames = lens _atLoadBalancerNames (\ s a -> s{_atLoadBalancerNames = a});

-- | The tags.
atTags :: Lens' AddTags (NonEmpty Tag)
atTags = lens _atTags (\ s a -> s{_atTags = a}) . _List1;

instance AWSRequest AddTags where
        type Sv AddTags = ELB
        type Rs AddTags = AddTagsResponse
        request = post
        response
          = receiveXMLWrapper "AddTagsResult"
              (\ s h x -> AddTagsResponse' <$> (pure s))

instance ToHeaders AddTags where
        toHeaders = const mempty

instance ToPath AddTags where
        toPath = const "/"

instance ToQuery AddTags where
        toQuery AddTags'{..}
          = mconcat
              ["Action" =: ("AddTags" :: ByteString),
               "Version" =: ("2012-06-01" :: ByteString),
               "LoadBalancerNames" =:
                 toQueryList "member" _atLoadBalancerNames,
               "Tags" =: toQueryList "member" _atTags]

-- | /See:/ 'addTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atrStatus'
newtype AddTagsResponse = AddTagsResponse'
    { _atrStatus :: Status
    } deriving (Eq,Read,Show)

-- | 'AddTagsResponse' smart constructor.
addTagsResponse :: Status -> AddTagsResponse
addTagsResponse pStatus =
    AddTagsResponse'
    { _atrStatus = pStatus
    }

-- | FIXME: Undocumented member.
atrStatus :: Lens' AddTagsResponse Status
atrStatus = lens _atrStatus (\ s a -> s{_atrStatus = a});
