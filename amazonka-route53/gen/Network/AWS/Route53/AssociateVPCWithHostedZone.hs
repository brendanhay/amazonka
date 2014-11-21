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

-- Module      : Network.AWS.Route53.AssociateVPCWithHostedZone
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action associates a VPC with an hosted zone. To associate a VPC with
-- an hosted zone, send a POST request to the 2013-04-01/hostedzone/hosted
-- zone ID/associatevpc resource. The request body must include an XML
-- document with a AssociateVPCWithHostedZoneRequest element. The response
-- returns the AssociateVPCWithHostedZoneResponse element that contains
-- ChangeInfo for you to track the progress of the
-- AssociateVPCWithHostedZoneRequest you made. See GetChange operation for how
-- to track the progress of your change.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_AssociateVPCWithHostedZone.html>
module Network.AWS.Route53.AssociateVPCWithHostedZone
    (
    -- * Request
      AssociateVPCWithHostedZone
    -- ** Request constructor
    , associateVPCWithHostedZone
    -- ** Request lenses
    , avpcwhzComment
    , avpcwhzHostedZoneId
    , avpcwhzVPC

    -- * Response
    , AssociateVPCWithHostedZoneResponse
    -- ** Response constructor
    , associateVPCWithHostedZoneResponse
    -- ** Response lenses
    , avpcwhzrChangeInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data AssociateVPCWithHostedZone = AssociateVPCWithHostedZone
    { _avpcwhzComment      :: Maybe Text
    , _avpcwhzHostedZoneId :: Text
    , _avpcwhzVPC          :: VPC
    } deriving (Eq, Show)

-- | 'AssociateVPCWithHostedZone' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avpcwhzComment' @::@ 'Maybe' 'Text'
--
-- * 'avpcwhzHostedZoneId' @::@ 'Text'
--
-- * 'avpcwhzVPC' @::@ 'VPC'
--
associateVPCWithHostedZone :: Text -- ^ 'avpcwhzHostedZoneId'
                           -> VPC -- ^ 'avpcwhzVPC'
                           -> AssociateVPCWithHostedZone
associateVPCWithHostedZone p1 p2 = AssociateVPCWithHostedZone
    { _avpcwhzHostedZoneId = p1
    , _avpcwhzVPC          = p2
    , _avpcwhzComment      = Nothing
    }

-- | Optional: Any comments you want to include about a
-- AssociateVPCWithHostedZoneRequest.
avpcwhzComment :: Lens' AssociateVPCWithHostedZone (Maybe Text)
avpcwhzComment = lens _avpcwhzComment (\s a -> s { _avpcwhzComment = a })

-- | The ID of the hosted zone you want to associate your VPC with. Note that
-- you cannot associate a VPC with a hosted zone that doesn't have an
-- existing VPC association.
avpcwhzHostedZoneId :: Lens' AssociateVPCWithHostedZone Text
avpcwhzHostedZoneId =
    lens _avpcwhzHostedZoneId (\s a -> s { _avpcwhzHostedZoneId = a })

-- | The VPC that you want your hosted zone to be associated with.
avpcwhzVPC :: Lens' AssociateVPCWithHostedZone VPC
avpcwhzVPC = lens _avpcwhzVPC (\s a -> s { _avpcwhzVPC = a })

newtype AssociateVPCWithHostedZoneResponse = AssociateVPCWithHostedZoneResponse
    { _avpcwhzrChangeInfo :: ChangeInfo
    } deriving (Eq, Show)

-- | 'AssociateVPCWithHostedZoneResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avpcwhzrChangeInfo' @::@ 'ChangeInfo'
--
associateVPCWithHostedZoneResponse :: ChangeInfo -- ^ 'avpcwhzrChangeInfo'
                                   -> AssociateVPCWithHostedZoneResponse
associateVPCWithHostedZoneResponse p1 = AssociateVPCWithHostedZoneResponse
    { _avpcwhzrChangeInfo = p1
    }

-- | A complex type that contains the ID, the status, and the date and time of
-- your AssociateVPCWithHostedZoneRequest.
avpcwhzrChangeInfo :: Lens' AssociateVPCWithHostedZoneResponse ChangeInfo
avpcwhzrChangeInfo =
    lens _avpcwhzrChangeInfo (\s a -> s { _avpcwhzrChangeInfo = a })

instance ToPath AssociateVPCWithHostedZone where
    toPath AssociateVPCWithHostedZone{..} = mconcat
        [ "/2013-04-01/hostedzone/"
        , toText _avpcwhzHostedZoneId
        , "/associatevpc"
        ]

instance ToQuery AssociateVPCWithHostedZone where
    toQuery = const mempty

instance ToHeaders AssociateVPCWithHostedZone

instance ToXMLRoot AssociateVPCWithHostedZone where
    toXMLRoot AssociateVPCWithHostedZone{..} = namespace ns "AssociateVPCWithHostedZone"
        [ "VPC"     =@ _avpcwhzVPC
        , "Comment" =@ _avpcwhzComment
        ]

instance ToXML AssociateVPCWithHostedZone

instance AWSRequest AssociateVPCWithHostedZone where
    type Sv AssociateVPCWithHostedZone = Route53
    type Rs AssociateVPCWithHostedZone = AssociateVPCWithHostedZoneResponse

    request  = post
    response = xmlResponse

instance FromXML AssociateVPCWithHostedZoneResponse where
    parseXML x = AssociateVPCWithHostedZoneResponse
        <$> x .@  "ChangeInfo"
