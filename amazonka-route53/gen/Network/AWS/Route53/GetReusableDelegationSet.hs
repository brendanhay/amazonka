{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetReusableDelegationSet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- To retrieve the reusable delegation set, send a @GET@ request to the
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
    , grdsrsStatus
    , grdsrsDelegationSet
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetReusableDelegationSet' smart constructor.
getReusableDelegationSet :: Text -> GetReusableDelegationSet
getReusableDelegationSet pId_ =
    GetReusableDelegationSet'
    { _grdsId = pId_
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
                   (pure (fromEnum s)) <*> (x .@ "DelegationSet"))

instance ToHeaders GetReusableDelegationSet where
        toHeaders = const mempty

instance ToPath GetReusableDelegationSet where
        toPath GetReusableDelegationSet'{..}
          = mconcat
              ["/2013-04-01/delegationset/", toPath _grdsId]

instance ToQuery GetReusableDelegationSet where
        toQuery = const mempty

-- | A complex type containing information about the specified reusable
-- delegation set.
--
-- /See:/ 'getReusableDelegationSetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grdsrsStatus'
--
-- * 'grdsrsDelegationSet'
data GetReusableDelegationSetResponse = GetReusableDelegationSetResponse'
    { _grdsrsStatus        :: !Int
    , _grdsrsDelegationSet :: !DelegationSet
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetReusableDelegationSetResponse' smart constructor.
getReusableDelegationSetResponse :: Int -> DelegationSet -> GetReusableDelegationSetResponse
getReusableDelegationSetResponse pStatus_ pDelegationSet_ =
    GetReusableDelegationSetResponse'
    { _grdsrsStatus = pStatus_
    , _grdsrsDelegationSet = pDelegationSet_
    }

-- | FIXME: Undocumented member.
grdsrsStatus :: Lens' GetReusableDelegationSetResponse Int
grdsrsStatus = lens _grdsrsStatus (\ s a -> s{_grdsrsStatus = a});

-- | A complex type that contains the information about the nameservers for
-- the specified delegation set ID.
grdsrsDelegationSet :: Lens' GetReusableDelegationSetResponse DelegationSet
grdsrsDelegationSet = lens _grdsrsDelegationSet (\ s a -> s{_grdsrsDelegationSet = a});
