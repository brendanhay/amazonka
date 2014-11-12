{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.RDS.ModifyDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an existing DB subnet group. DB subnet groups must contain at
-- least one subnet in at least two AZs in the region.
module Network.AWS.RDS.ModifyDBSubnetGroup
    (
    -- * Request
      ModifyDBSubnetGroupMessage
    -- ** Request constructor
    , modifyDBSubnetGroupMessage
    -- ** Request lenses
    , mdbsgmDBSubnetGroupDescription
    , mdbsgmDBSubnetGroupName
    , mdbsgmSubnetIds

    -- * Response
    , ModifyDBSubnetGroupResult
    -- ** Response constructor
    , modifyDBSubnetGroupResult
    -- ** Response lenses
    , mdbsgrDBSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data ModifyDBSubnetGroupMessage = ModifyDBSubnetGroupMessage
    { _mdbsgmDBSubnetGroupDescription :: Maybe Text
    , _mdbsgmDBSubnetGroupName        :: Text
    , _mdbsgmSubnetIds                :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ModifyDBSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbsgmDBSubnetGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'mdbsgmDBSubnetGroupName' @::@ 'Text'
--
-- * 'mdbsgmSubnetIds' @::@ ['Text']
--
modifyDBSubnetGroupMessage :: Text -- ^ 'mdbsgmDBSubnetGroupName'
                           -> ModifyDBSubnetGroupMessage
modifyDBSubnetGroupMessage p1 = ModifyDBSubnetGroupMessage
    { _mdbsgmDBSubnetGroupName        = p1
    , _mdbsgmDBSubnetGroupDescription = Nothing
    , _mdbsgmSubnetIds                = mempty
    }

-- | The description for the DB subnet group.
mdbsgmDBSubnetGroupDescription :: Lens' ModifyDBSubnetGroupMessage (Maybe Text)
mdbsgmDBSubnetGroupDescription =
    lens _mdbsgmDBSubnetGroupDescription
        (\s a -> s { _mdbsgmDBSubnetGroupDescription = a })

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Must not be "Default". Example: mySubnetgroup.
mdbsgmDBSubnetGroupName :: Lens' ModifyDBSubnetGroupMessage Text
mdbsgmDBSubnetGroupName =
    lens _mdbsgmDBSubnetGroupName (\s a -> s { _mdbsgmDBSubnetGroupName = a })

-- | The EC2 subnet IDs for the DB subnet group.
mdbsgmSubnetIds :: Lens' ModifyDBSubnetGroupMessage [Text]
mdbsgmSubnetIds = lens _mdbsgmSubnetIds (\s a -> s { _mdbsgmSubnetIds = a })

instance ToQuery ModifyDBSubnetGroupMessage

instance ToPath ModifyDBSubnetGroupMessage where
    toPath = const "/"

newtype ModifyDBSubnetGroupResult = ModifyDBSubnetGroupResult
    { _mdbsgrDBSubnetGroup :: Maybe DBSubnetGroup
    } deriving (Eq, Show, Generic)

-- | 'ModifyDBSubnetGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbsgrDBSubnetGroup' @::@ 'Maybe' 'DBSubnetGroup'
--
modifyDBSubnetGroupResult :: ModifyDBSubnetGroupResult
modifyDBSubnetGroupResult = ModifyDBSubnetGroupResult
    { _mdbsgrDBSubnetGroup = Nothing
    }

mdbsgrDBSubnetGroup :: Lens' ModifyDBSubnetGroupResult (Maybe DBSubnetGroup)
mdbsgrDBSubnetGroup =
    lens _mdbsgrDBSubnetGroup (\s a -> s { _mdbsgrDBSubnetGroup = a })

instance FromXML ModifyDBSubnetGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyDBSubnetGroupResult"

instance AWSRequest ModifyDBSubnetGroupMessage where
    type Sv ModifyDBSubnetGroupMessage = RDS
    type Rs ModifyDBSubnetGroupMessage = ModifyDBSubnetGroupResult

    request  = post "ModifyDBSubnetGroup"
    response = xmlResponse $ \h x -> ModifyDBSubnetGroupResult
        <$> x %| "DBSubnetGroup"
