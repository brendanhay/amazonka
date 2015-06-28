{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.GetReusableDelegationSet
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

-- | To retrieve the reusable delegation set, send a @GET@ request to the
-- @2013-04-01\/delegationset\/delegation set ID@ resource.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetReusableDelegationSet.html>
module Network.AWS.Route53.GetReusableDelegationSet
    (
    -- * Request
      GetReusableDelegationSet
    -- ** Request constructor
    , getReusableDelegationSet
    -- ** Request lenses
    , grdsId

    -- * Response
    , GetReusableDelegationSetResponse
    -- ** Response constructor
    , getReusableDelegationSetResponse
    -- ** Response lenses
    , grdsrDelegationSet
    , grdsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | The input for a GetReusableDelegationSet request.
--
-- /See:/ 'getReusableDelegationSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grdsId'
newtype GetReusableDelegationSet = GetReusableDelegationSet'
    { _grdsId :: Text
    } deriving (Eq,Read,Show)

-- | 'GetReusableDelegationSet' smart constructor.
getReusableDelegationSet :: Text -> GetReusableDelegationSet
getReusableDelegationSet pId =
    GetReusableDelegationSet'
    { _grdsId = pId
    }

-- | The ID of the reusable delegation set for which you want to get a list
-- of the name server.
grdsId :: Lens' GetReusableDelegationSet Text
grdsId = lens _grdsId (\ s a -> s{_grdsId = a});

instance AWSRequest GetReusableDelegationSet where
        type Sv GetReusableDelegationSet = Route53
        type Rs GetReusableDelegationSet =
             GetReusableDelegationSetResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetReusableDelegationSetResponse' <$>
                   (x .@ "DelegationSet") <*> (pure s))

instance ToHeaders GetReusableDelegationSet where
        toHeaders = const mempty

instance ToPath GetReusableDelegationSet where
        toPath GetReusableDelegationSet'{..}
          = mconcat
              ["/2013-04-01/delegationset/", toText _grdsId]

instance ToQuery GetReusableDelegationSet where
        toQuery = const mempty

-- | A complex type containing information about the specified reusable
-- delegation set.
--
-- /See:/ 'getReusableDelegationSetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grdsrDelegationSet'
--
-- * 'grdsrStatus'
data GetReusableDelegationSetResponse = GetReusableDelegationSetResponse'
    { _grdsrDelegationSet :: !DelegationSet
    , _grdsrStatus        :: !Status
    } deriving (Eq,Read,Show)

-- | 'GetReusableDelegationSetResponse' smart constructor.
getReusableDelegationSetResponse :: DelegationSet -> Status -> GetReusableDelegationSetResponse
getReusableDelegationSetResponse pDelegationSet pStatus =
    GetReusableDelegationSetResponse'
    { _grdsrDelegationSet = pDelegationSet
    , _grdsrStatus = pStatus
    }

-- | A complex type that contains the information about the nameservers for
-- the specified delegation set ID.
grdsrDelegationSet :: Lens' GetReusableDelegationSetResponse DelegationSet
grdsrDelegationSet = lens _grdsrDelegationSet (\ s a -> s{_grdsrDelegationSet = a});

-- | FIXME: Undocumented member.
grdsrStatus :: Lens' GetReusableDelegationSetResponse Status
grdsrStatus = lens _grdsrStatus (\ s a -> s{_grdsrStatus = a});
