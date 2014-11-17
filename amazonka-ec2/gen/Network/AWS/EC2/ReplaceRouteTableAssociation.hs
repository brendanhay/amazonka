{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ReplaceRouteTableAssociation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the route table associated with a given subnet in a VPC. After the
-- operation completes, the subnet uses the routes in the new route table it's
-- associated with. For more information about route tables, see Route Tables
-- in the Amazon Virtual Private Cloud User Guide. You can also use
-- ReplaceRouteTableAssociation to change which table is the main route table
-- in the VPC. You just specify the main route table's association ID and the
-- route table to be the new main route table.
module Network.AWS.EC2.ReplaceRouteTableAssociation
    (
    -- * Request
      ReplaceRouteTableAssociation
    -- ** Request constructor
    , replaceRouteTableAssociation
    -- ** Request lenses
    , rrtaAssociationId
    , rrtaDryRun
    , rrtaRouteTableId

    -- * Response
    , ReplaceRouteTableAssociationResponse
    -- ** Response constructor
    , replaceRouteTableAssociationResponse
    -- ** Response lenses
    , rrtarNewAssociationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
    { _rrtaAssociationId :: Text
    , _rrtaDryRun        :: Maybe Bool
    , _rrtaRouteTableId  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ReplaceRouteTableAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrtaAssociationId' @::@ 'Text'
--
-- * 'rrtaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rrtaRouteTableId' @::@ 'Text'
--
replaceRouteTableAssociation :: Text -- ^ 'rrtaAssociationId'
                             -> Text -- ^ 'rrtaRouteTableId'
                             -> ReplaceRouteTableAssociation
replaceRouteTableAssociation p1 p2 = ReplaceRouteTableAssociation
    { _rrtaAssociationId = p1
    , _rrtaRouteTableId  = p2
    , _rrtaDryRun        = Nothing
    }

-- | The association ID.
rrtaAssociationId :: Lens' ReplaceRouteTableAssociation Text
rrtaAssociationId =
    lens _rrtaAssociationId (\s a -> s { _rrtaAssociationId = a })

rrtaDryRun :: Lens' ReplaceRouteTableAssociation (Maybe Bool)
rrtaDryRun = lens _rrtaDryRun (\s a -> s { _rrtaDryRun = a })

-- | The ID of the new route table to associate with the subnet.
rrtaRouteTableId :: Lens' ReplaceRouteTableAssociation Text
rrtaRouteTableId = lens _rrtaRouteTableId (\s a -> s { _rrtaRouteTableId = a })

newtype ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
    { _rrtarNewAssociationId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ReplaceRouteTableAssociationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrtarNewAssociationId' @::@ 'Maybe' 'Text'
--
replaceRouteTableAssociationResponse :: ReplaceRouteTableAssociationResponse
replaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
    { _rrtarNewAssociationId = Nothing
    }

-- | The ID of the new association.
rrtarNewAssociationId :: Lens' ReplaceRouteTableAssociationResponse (Maybe Text)
rrtarNewAssociationId =
    lens _rrtarNewAssociationId (\s a -> s { _rrtarNewAssociationId = a })

instance AWSRequest ReplaceRouteTableAssociation where
    type Sv ReplaceRouteTableAssociation = EC2
    type Rs ReplaceRouteTableAssociation = ReplaceRouteTableAssociationResponse

    request  = post "ReplaceRouteTableAssociation"
    response = xmlResponse

instance FromXML ReplaceRouteTableAssociationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplaceRouteTableAssociationResponse"

instance ToPath ReplaceRouteTableAssociation where
    toPath = const "/"

instance ToHeaders ReplaceRouteTableAssociation

instance ToQuery ReplaceRouteTableAssociation
