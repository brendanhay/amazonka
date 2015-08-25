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
-- Module      : Network.AWS.ELB.AddTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified load balancer. Each load
-- balancer can have a maximum of 10 tags.
--
-- Each tag consists of a key and an optional value. If a tag with the same
-- key is already associated with the load balancer, 'AddTags' updates its
-- value.
--
-- For more information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/add-remove-tags.html Tag Your Load Balancer>
-- in the /Elastic Load Balancing Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/APIReference/API_AddTags.html AWS API Reference> for AddTags.
module Network.AWS.ELB.AddTags
    (
    -- * Creating a Request
      addTags
    , AddTags
    -- * Request Lenses
    , atLoadBalancerNames
    , atTags

    -- * Destructuring the Response
    , addTagsResponse
    , AddTagsResponse
    -- * Response Lenses
    , atrsStatus
    ) where

import           Network.AWS.ELB.Types
import           Network.AWS.ELB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addTags' smart constructor.
data AddTags = AddTags'
    { _atLoadBalancerNames :: ![Text]
    , _atTags              :: !(List1 Tag)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atLoadBalancerNames'
--
-- * 'atTags'
addTags
    :: NonEmpty Tag -- ^ 'atTags'
    -> AddTags
addTags pTags_ =
    AddTags'
    { _atLoadBalancerNames = mempty
    , _atTags = _List1 # pTags_
    }

-- | The name of the load balancer. You can specify one load balancer only.
atLoadBalancerNames :: Lens' AddTags [Text]
atLoadBalancerNames = lens _atLoadBalancerNames (\ s a -> s{_atLoadBalancerNames = a}) . _Coerce;

-- | The tags.
atTags :: Lens' AddTags (NonEmpty Tag)
atTags = lens _atTags (\ s a -> s{_atTags = a}) . _List1;

instance AWSRequest AddTags where
        type Rs AddTags = AddTagsResponse
        request = postQuery eLB
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
                 toQueryList "member" _atLoadBalancerNames,
               "Tags" =: toQueryList "member" _atTags]

-- | /See:/ 'addTagsResponse' smart constructor.
newtype AddTagsResponse = AddTagsResponse'
    { _atrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atrsStatus'
addTagsResponse
    :: Int -- ^ 'atrsStatus'
    -> AddTagsResponse
addTagsResponse pStatus_ =
    AddTagsResponse'
    { _atrsStatus = pStatus_
    }

-- | The response status code.
atrsStatus :: Lens' AddTagsResponse Int
atrsStatus = lens _atrsStatus (\ s a -> s{_atrsStatus = a});
