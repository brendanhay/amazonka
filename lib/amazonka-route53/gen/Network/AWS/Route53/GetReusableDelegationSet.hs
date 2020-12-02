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
-- Module      : Network.AWS.Route53.GetReusableDelegationSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified reusable delegation set, including the four name servers that are assigned to the delegation set.
--
--
module Network.AWS.Route53.GetReusableDelegationSet
    (
    -- * Creating a Request
      getReusableDelegationSet
    , GetReusableDelegationSet
    -- * Request Lenses
    , grdsId

    -- * Destructuring the Response
    , getReusableDelegationSetResponse
    , GetReusableDelegationSetResponse
    -- * Response Lenses
    , grdsrsResponseStatus
    , grdsrsDelegationSet
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to get information about a specified reusable delegation set.
--
--
--
-- /See:/ 'getReusableDelegationSet' smart constructor.
newtype GetReusableDelegationSet = GetReusableDelegationSet'
  { _grdsId :: ResourceId
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReusableDelegationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdsId' - The ID of the reusable delegation set that you want to get a list of name servers for.
getReusableDelegationSet
    :: ResourceId -- ^ 'grdsId'
    -> GetReusableDelegationSet
getReusableDelegationSet pId_ = GetReusableDelegationSet' {_grdsId = pId_}


-- | The ID of the reusable delegation set that you want to get a list of name servers for.
grdsId :: Lens' GetReusableDelegationSet ResourceId
grdsId = lens _grdsId (\ s a -> s{_grdsId = a})

instance AWSRequest GetReusableDelegationSet where
        type Rs GetReusableDelegationSet =
             GetReusableDelegationSetResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetReusableDelegationSetResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "DelegationSet"))

instance Hashable GetReusableDelegationSet where

instance NFData GetReusableDelegationSet where

instance ToHeaders GetReusableDelegationSet where
        toHeaders = const mempty

instance ToPath GetReusableDelegationSet where
        toPath GetReusableDelegationSet'{..}
          = mconcat
              ["/2013-04-01/delegationset/", toBS _grdsId]

instance ToQuery GetReusableDelegationSet where
        toQuery = const mempty

-- | A complex type that contains the response to the @GetReusableDelegationSet@ request.
--
--
--
-- /See:/ 'getReusableDelegationSetResponse' smart constructor.
data GetReusableDelegationSetResponse = GetReusableDelegationSetResponse'
  { _grdsrsResponseStatus :: !Int
  , _grdsrsDelegationSet  :: !DelegationSet
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReusableDelegationSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdsrsResponseStatus' - -- | The response status code.
--
-- * 'grdsrsDelegationSet' - A complex type that contains information about the reusable delegation set.
getReusableDelegationSetResponse
    :: Int -- ^ 'grdsrsResponseStatus'
    -> DelegationSet -- ^ 'grdsrsDelegationSet'
    -> GetReusableDelegationSetResponse
getReusableDelegationSetResponse pResponseStatus_ pDelegationSet_ =
  GetReusableDelegationSetResponse'
    { _grdsrsResponseStatus = pResponseStatus_
    , _grdsrsDelegationSet = pDelegationSet_
    }


-- | -- | The response status code.
grdsrsResponseStatus :: Lens' GetReusableDelegationSetResponse Int
grdsrsResponseStatus = lens _grdsrsResponseStatus (\ s a -> s{_grdsrsResponseStatus = a})

-- | A complex type that contains information about the reusable delegation set.
grdsrsDelegationSet :: Lens' GetReusableDelegationSetResponse DelegationSet
grdsrsDelegationSet = lens _grdsrsDelegationSet (\ s a -> s{_grdsrsDelegationSet = a})

instance NFData GetReusableDelegationSetResponse
         where
