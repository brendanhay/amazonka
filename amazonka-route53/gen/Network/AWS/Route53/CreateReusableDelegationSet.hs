{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.CreateReusableDelegationSet
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

-- | This action creates a reusable delegationSet.
--
-- To create a new reusable delegationSet, send a @POST@ request to the
-- @2013-04-01\/delegationset@ resource. The request body must include an
-- XML document with a @CreateReusableDelegationSetRequest@ element. The
-- response returns the @CreateReusableDelegationSetResponse@ element that
-- contains metadata about the delegationSet.
--
-- If the optional parameter HostedZoneId is specified, it marks the
-- delegationSet associated with that particular hosted zone as reusable.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateReusableDelegationSet.html>
module Network.AWS.Route53.CreateReusableDelegationSet
    (
    -- * Request
      CreateReusableDelegationSet
    -- ** Request constructor
    , createReusableDelegationSet
    -- ** Request lenses
    , crdsHostedZoneId
    , crdsCallerReference

    -- * Response
    , CreateReusableDelegationSetResponse
    -- ** Response constructor
    , createReusableDelegationSetResponse
    -- ** Response lenses
    , crdsrDelegationSet
    , crdsrLocation
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Route53.Types

-- | /See:/ 'createReusableDelegationSet' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crdsHostedZoneId'
--
-- * 'crdsCallerReference'
data CreateReusableDelegationSet = CreateReusableDelegationSet'{_crdsHostedZoneId :: Maybe Text, _crdsCallerReference :: Text} deriving (Eq, Read, Show)

-- | 'CreateReusableDelegationSet' smart constructor.
createReusableDelegationSet :: Text -> CreateReusableDelegationSet
createReusableDelegationSet pCallerReference = CreateReusableDelegationSet'{_crdsHostedZoneId = Nothing, _crdsCallerReference = pCallerReference};

-- | The ID of the hosted zone whose delegation set you want to mark as
-- reusable. It is an optional parameter.
crdsHostedZoneId :: Lens' CreateReusableDelegationSet (Maybe Text)
crdsHostedZoneId = lens _crdsHostedZoneId (\ s a -> s{_crdsHostedZoneId = a});

-- | A unique string that identifies the request and that allows failed
-- @CreateReusableDelegationSet@ requests to be retried without the risk of
-- executing the operation twice. You must use a unique @CallerReference@
-- string every time you create a reusable delegation set.
-- @CallerReference@ can be any unique string; you might choose to use a
-- string that identifies your project, such as @DNSMigration_01@.
--
-- Valid characters are any Unicode code points that are legal in an XML
-- 1.0 document. The UTF-8 encoding of the value must be less than 128
-- bytes.
crdsCallerReference :: Lens' CreateReusableDelegationSet Text
crdsCallerReference = lens _crdsCallerReference (\ s a -> s{_crdsCallerReference = a});

instance AWSRequest CreateReusableDelegationSet where
        type Sv CreateReusableDelegationSet = Route53
        type Rs CreateReusableDelegationSet =
             CreateReusableDelegationSetResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 CreateReusableDelegationSetResponse' <$>
                   x .@ "DelegationSet" <*> h .# "Location")

instance ToElement CreateReusableDelegationSet where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateReusableDelegationSetRequest"

instance ToHeaders CreateReusableDelegationSet where
        toHeaders = const mempty

instance ToPath CreateReusableDelegationSet where
        toPath = const "/2013-04-01/delegationset"

instance ToQuery CreateReusableDelegationSet where
        toQuery = const mempty

instance ToXML CreateReusableDelegationSet where
        toXML CreateReusableDelegationSet'{..}
          = mconcat
              ["HostedZoneId" @= _crdsHostedZoneId,
               "CallerReference" @= _crdsCallerReference]

-- | /See:/ 'createReusableDelegationSetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crdsrDelegationSet'
--
-- * 'crdsrLocation'
data CreateReusableDelegationSetResponse = CreateReusableDelegationSetResponse'{_crdsrDelegationSet :: DelegationSet, _crdsrLocation :: Text} deriving (Eq, Read, Show)

-- | 'CreateReusableDelegationSetResponse' smart constructor.
createReusableDelegationSetResponse :: DelegationSet -> Text -> CreateReusableDelegationSetResponse
createReusableDelegationSetResponse pDelegationSet pLocation = CreateReusableDelegationSetResponse'{_crdsrDelegationSet = pDelegationSet, _crdsrLocation = pLocation};

-- | A complex type that contains name server information.
crdsrDelegationSet :: Lens' CreateReusableDelegationSetResponse DelegationSet
crdsrDelegationSet = lens _crdsrDelegationSet (\ s a -> s{_crdsrDelegationSet = a});

-- | The unique URL representing the new reusbale delegation set.
crdsrLocation :: Lens' CreateReusableDelegationSetResponse Text
crdsrLocation = lens _crdsrLocation (\ s a -> s{_crdsrLocation = a});
