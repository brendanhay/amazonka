{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.CreateReusableDelegationSet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
-- To create a new reusable delegationSet, send a 'POST' request to the '2013-04-01/delegationset' resource. The request body must include an XML document with a 'CreateReusableDelegationSetRequest' element. The response returns the 'CreateReusableDelegationSetResponse'
-- element that contains metadata about the delegationSet.
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
    , crdsCallerReference
    , crdsHostedZoneId

    -- * Response
    , CreateReusableDelegationSetResponse
    -- ** Response constructor
    , createReusableDelegationSetResponse
    -- ** Response lenses
    , crdsrDelegationSet
    , crdsrLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data CreateReusableDelegationSet = CreateReusableDelegationSet
    { _crdsCallerReference :: Text
    , _crdsHostedZoneId    :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'CreateReusableDelegationSet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crdsCallerReference' @::@ 'Text'
--
-- * 'crdsHostedZoneId' @::@ 'Maybe' 'Text'
--
createReusableDelegationSet :: Text -- ^ 'crdsCallerReference'
                            -> CreateReusableDelegationSet
createReusableDelegationSet p1 = CreateReusableDelegationSet
    { _crdsCallerReference = p1
    , _crdsHostedZoneId    = Nothing
    }

-- | A unique string that identifies the request and that allows failed 'CreateReusableDelegationSet' requests to be retried without the risk of executing the operation twice.
-- You must use a unique 'CallerReference' string every time you create a reusable
-- delegation set. 'CallerReference' can be any unique string; you might choose to
-- use a string that identifies your project, such as 'DNSMigration_01'.
--
-- Valid characters are any Unicode code points that are legal in an XML 1.0
-- document. The UTF-8 encoding of the value must be less than 128 bytes.
crdsCallerReference :: Lens' CreateReusableDelegationSet Text
crdsCallerReference =
    lens _crdsCallerReference (\s a -> s { _crdsCallerReference = a })

-- | The ID of the hosted zone whose delegation set you want to mark as reusable.
-- It is an optional parameter.
crdsHostedZoneId :: Lens' CreateReusableDelegationSet (Maybe Text)
crdsHostedZoneId = lens _crdsHostedZoneId (\s a -> s { _crdsHostedZoneId = a })

data CreateReusableDelegationSetResponse = CreateReusableDelegationSetResponse
    { _crdsrDelegationSet :: DelegationSet
    , _crdsrLocation      :: Text
    } deriving (Eq, Show)

-- | 'CreateReusableDelegationSetResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crdsrDelegationSet' @::@ 'DelegationSet'
--
-- * 'crdsrLocation' @::@ 'Text'
--
createReusableDelegationSetResponse :: DelegationSet -- ^ 'crdsrDelegationSet'
                                    -> Text -- ^ 'crdsrLocation'
                                    -> CreateReusableDelegationSetResponse
createReusableDelegationSetResponse p1 p2 = CreateReusableDelegationSetResponse
    { _crdsrDelegationSet = p1
    , _crdsrLocation      = p2
    }

-- | A complex type that contains name server information.
crdsrDelegationSet :: Lens' CreateReusableDelegationSetResponse DelegationSet
crdsrDelegationSet =
    lens _crdsrDelegationSet (\s a -> s { _crdsrDelegationSet = a })

-- | The unique URL representing the new reusbale delegation set.
crdsrLocation :: Lens' CreateReusableDelegationSetResponse Text
crdsrLocation = lens _crdsrLocation (\s a -> s { _crdsrLocation = a })

instance ToPath CreateReusableDelegationSet where
    toPath = const "/2013-04-01/delegationset"

instance ToQuery CreateReusableDelegationSet where
    toQuery = const mempty

instance ToHeaders CreateReusableDelegationSet

instance ToXMLRoot CreateReusableDelegationSet where
    toXMLRoot CreateReusableDelegationSet{..} = namespaced ns "CreateReusableDelegationSet"
        [ "CallerReference" =@ _crdsCallerReference
        , "HostedZoneId"    =@ _crdsHostedZoneId
        ]

instance ToXML CreateReusableDelegationSet

instance AWSRequest CreateReusableDelegationSet where
    type Sv CreateReusableDelegationSet = Route53
    type Rs CreateReusableDelegationSet = CreateReusableDelegationSetResponse

    request  = post
    response = xmlHeaderResponse $ \h x -> CreateReusableDelegationSetResponse
        <$> x .@  "DelegationSet" "DelegationSet"
        <*> h ~: "Location"
