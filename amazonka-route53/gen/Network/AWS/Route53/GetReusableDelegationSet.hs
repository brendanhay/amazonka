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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve the reusable delegation set, send a 'GET' request to the
-- '\/Route 53 API version\/delegationset\/delegation set ID' resource.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetReusableDelegationSet.html AWS API Reference> for GetReusableDelegationSet.
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | The input for a 'GetReusableDelegationSet' request.
--
-- /See:/ 'getReusableDelegationSet' smart constructor.
newtype GetReusableDelegationSet = GetReusableDelegationSet'
    { _grdsId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetReusableDelegationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdsId'
getReusableDelegationSet
    :: Text -- ^ 'grdsId'
    -> GetReusableDelegationSet
getReusableDelegationSet pId_ =
    GetReusableDelegationSet'
    { _grdsId = pId_
    }

-- | The ID of the reusable delegation set for which you want to get a list
-- of the name server.
grdsId :: Lens' GetReusableDelegationSet Text
grdsId = lens _grdsId (\ s a -> s{_grdsId = a});

instance AWSRequest GetReusableDelegationSet where
        type Rs GetReusableDelegationSet =
             GetReusableDelegationSetResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetReusableDelegationSetResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "DelegationSet"))

instance ToHeaders GetReusableDelegationSet where
        toHeaders = const mempty

instance ToPath GetReusableDelegationSet where
        toPath GetReusableDelegationSet'{..}
          = mconcat
              ["/2013-04-01/delegationset/", toBS _grdsId]

instance ToQuery GetReusableDelegationSet where
        toQuery = const mempty

-- | A complex type containing information about the specified reusable
-- delegation set.
--
-- /See:/ 'getReusableDelegationSetResponse' smart constructor.
data GetReusableDelegationSetResponse = GetReusableDelegationSetResponse'
    { _grdsrsResponseStatus :: !Int
    , _grdsrsDelegationSet  :: !DelegationSet
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetReusableDelegationSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdsrsResponseStatus'
--
-- * 'grdsrsDelegationSet'
getReusableDelegationSetResponse
    :: Int -- ^ 'grdsrsResponseStatus'
    -> DelegationSet -- ^ 'grdsrsDelegationSet'
    -> GetReusableDelegationSetResponse
getReusableDelegationSetResponse pResponseStatus_ pDelegationSet_ =
    GetReusableDelegationSetResponse'
    { _grdsrsResponseStatus = pResponseStatus_
    , _grdsrsDelegationSet = pDelegationSet_
    }

-- | The response status code.
grdsrsResponseStatus :: Lens' GetReusableDelegationSetResponse Int
grdsrsResponseStatus = lens _grdsrsResponseStatus (\ s a -> s{_grdsrsResponseStatus = a});

-- | A complex type that contains the information about the nameservers for
-- the specified delegation set ID.
grdsrsDelegationSet :: Lens' GetReusableDelegationSetResponse DelegationSet
grdsrsDelegationSet = lens _grdsrsDelegationSet (\ s a -> s{_grdsrsDelegationSet = a});
