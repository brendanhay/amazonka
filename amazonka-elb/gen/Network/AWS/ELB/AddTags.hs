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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified tags to the specified load balancer. Each load balancer can have a maximum of 10 tags.
--
--
-- Each tag consists of a key and an optional value. If a tag with the same key is already associated with the load balancer, @AddTags@ updates its value.
--
-- For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html Tag Your Classic Load Balancer> in the /Classic Load Balancer Guide/ .
--
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
    , atrsResponseStatus
    ) where

import Network.AWS.ELB.Types
import Network.AWS.ELB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for AddTags.
--
--
--
-- /See:/ 'addTags' smart constructor.
data AddTags = AddTags'
  { _atLoadBalancerNames :: ![Text]
  , _atTags              :: !(List1 Tag)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atLoadBalancerNames' - The name of the load balancer. You can specify one load balancer only.
--
-- * 'atTags' - The tags.
addTags
    :: NonEmpty Tag -- ^ 'atTags'
    -> AddTags
addTags pTags_ =
  AddTags' {_atLoadBalancerNames = mempty, _atTags = _List1 # pTags_}


-- | The name of the load balancer. You can specify one load balancer only.
atLoadBalancerNames :: Lens' AddTags [Text]
atLoadBalancerNames = lens _atLoadBalancerNames (\ s a -> s{_atLoadBalancerNames = a}) . _Coerce

-- | The tags.
atTags :: Lens' AddTags (NonEmpty Tag)
atTags = lens _atTags (\ s a -> s{_atTags = a}) . _List1

instance AWSRequest AddTags where
        type Rs AddTags = AddTagsResponse
        request = postQuery elb
        response
          = receiveXMLWrapper "AddTagsResult"
              (\ s h x -> AddTagsResponse' <$> (pure (fromEnum s)))

instance Hashable AddTags where

instance NFData AddTags where

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

-- | Contains the output of AddTags.
--
--
--
-- /See:/ 'addTagsResponse' smart constructor.
newtype AddTagsResponse = AddTagsResponse'
  { _atrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atrsResponseStatus' - -- | The response status code.
addTagsResponse
    :: Int -- ^ 'atrsResponseStatus'
    -> AddTagsResponse
addTagsResponse pResponseStatus_ =
  AddTagsResponse' {_atrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
atrsResponseStatus :: Lens' AddTagsResponse Int
atrsResponseStatus = lens _atrsResponseStatus (\ s a -> s{_atrsResponseStatus = a})

instance NFData AddTagsResponse where
