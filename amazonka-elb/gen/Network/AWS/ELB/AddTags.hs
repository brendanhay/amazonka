{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.AddTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified load balancer. Each load
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
    , atrqLoadBalancerNames
    , atrqTags

    -- * Response
    , AddTagsResponse
    -- ** Response constructor
    , addTagsResponse
    -- ** Response lenses
    , atrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atrqLoadBalancerNames'
--
-- * 'atrqTags'
data AddTags = AddTags'
    { _atrqLoadBalancerNames :: ![Text]
    , _atrqTags              :: !(List1 Tag)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTags' smart constructor.
addTags :: NonEmpty Tag -> AddTags
addTags pTags_ =
    AddTags'
    { _atrqLoadBalancerNames = mempty
    , _atrqTags = _List1 # pTags_
    }

-- | The name of the load balancer. You can specify one load balancer only.
atrqLoadBalancerNames :: Lens' AddTags [Text]
atrqLoadBalancerNames = lens _atrqLoadBalancerNames (\ s a -> s{_atrqLoadBalancerNames = a});

-- | The tags.
atrqTags :: Lens' AddTags (NonEmpty Tag)
atrqTags = lens _atrqTags (\ s a -> s{_atrqTags = a}) . _List1;

instance AWSRequest AddTags where
        type Sv AddTags = ELB
        type Rs AddTags = AddTagsResponse
        request = post
        response
          = receiveXMLWrapper "AddTagsResult"
              (\ s h x -> AddTagsResponse' <$> (pure (fromEnum s)))

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
                 toQueryList "member" _atrqLoadBalancerNames,
               "Tags" =: toQueryList "member" _atrqTags]

-- | /See:/ 'addTagsResponse' smart constructor.
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
