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

-- Module      : Network.AWS.RDS.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB subnet group. DB subnet groups must contain at least one
-- subnet in at least two AZs in the region.
module Network.AWS.RDS.CreateDBSubnetGroup
    (
    -- * Request
      CreateDBSubnetGroupMessage
    -- ** Request constructor
    , createDBSubnetGroupMessage
    -- ** Request lenses
    , cdbsgmDBSubnetGroupDescription
    , cdbsgmDBSubnetGroupName
    , cdbsgmSubnetIds
    , cdbsgmTags

    -- * Response
    , CreateDBSubnetGroupResult
    -- ** Response constructor
    , createDBSubnetGroupResult
    -- ** Response lenses
    , cdbsgrDBSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CreateDBSubnetGroupMessage = CreateDBSubnetGroupMessage
    { _cdbsgmDBSubnetGroupDescription :: Text
    , _cdbsgmDBSubnetGroupName        :: Text
    , _cdbsgmSubnetIds                :: [Text]
    , _cdbsgmTags                     :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateDBSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsgmDBSubnetGroupDescription' @::@ 'Text'
--
-- * 'cdbsgmDBSubnetGroupName' @::@ 'Text'
--
-- * 'cdbsgmSubnetIds' @::@ ['Text']
--
-- * 'cdbsgmTags' @::@ ['Tag']
--
createDBSubnetGroupMessage :: Text -- ^ 'cdbsgmDBSubnetGroupName'
                           -> Text -- ^ 'cdbsgmDBSubnetGroupDescription'
                           -> CreateDBSubnetGroupMessage
createDBSubnetGroupMessage p1 p2 = CreateDBSubnetGroupMessage
    { _cdbsgmDBSubnetGroupName        = p1
    , _cdbsgmDBSubnetGroupDescription = p2
    , _cdbsgmSubnetIds                = mempty
    , _cdbsgmTags                     = mempty
    }

-- | The description for the DB subnet group.
cdbsgmDBSubnetGroupDescription :: Lens' CreateDBSubnetGroupMessage Text
cdbsgmDBSubnetGroupDescription =
    lens _cdbsgmDBSubnetGroupDescription
        (\s a -> s { _cdbsgmDBSubnetGroupDescription = a })

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Must not be "Default". Example: mySubnetgroup.
cdbsgmDBSubnetGroupName :: Lens' CreateDBSubnetGroupMessage Text
cdbsgmDBSubnetGroupName =
    lens _cdbsgmDBSubnetGroupName (\s a -> s { _cdbsgmDBSubnetGroupName = a })

-- | The EC2 Subnet IDs for the DB subnet group.
cdbsgmSubnetIds :: Lens' CreateDBSubnetGroupMessage [Text]
cdbsgmSubnetIds = lens _cdbsgmSubnetIds (\s a -> s { _cdbsgmSubnetIds = a })

cdbsgmTags :: Lens' CreateDBSubnetGroupMessage [Tag]
cdbsgmTags = lens _cdbsgmTags (\s a -> s { _cdbsgmTags = a })

instance ToPath CreateDBSubnetGroupMessage where
    toPath = const "/"

instance ToQuery CreateDBSubnetGroupMessage

newtype CreateDBSubnetGroupResult = CreateDBSubnetGroupResult
    { _cdbsgrDBSubnetGroup :: Maybe DBSubnetGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateDBSubnetGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsgrDBSubnetGroup' @::@ 'Maybe' 'DBSubnetGroup'
--
createDBSubnetGroupResult :: CreateDBSubnetGroupResult
createDBSubnetGroupResult = CreateDBSubnetGroupResult
    { _cdbsgrDBSubnetGroup = Nothing
    }

cdbsgrDBSubnetGroup :: Lens' CreateDBSubnetGroupResult (Maybe DBSubnetGroup)
cdbsgrDBSubnetGroup =
    lens _cdbsgrDBSubnetGroup (\s a -> s { _cdbsgrDBSubnetGroup = a })

instance AWSRequest CreateDBSubnetGroupMessage where
    type Sv CreateDBSubnetGroupMessage = RDS
    type Rs CreateDBSubnetGroupMessage = CreateDBSubnetGroupResult

    request  = post "CreateDBSubnetGroup"
    response = const . xmlResponse $ \h x -> CreateDBSubnetGroupResult
newtype
