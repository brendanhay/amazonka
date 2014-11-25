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

-- Module      : Network.AWS.RDS.ModifyDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an existing DB subnet group. DB subnet groups must contain at least
-- one subnet in at least two AZs in the region.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyDBSubnetGroup.html>
module Network.AWS.RDS.ModifyDBSubnetGroup
    (
    -- * Request
      ModifyDBSubnetGroup
    -- ** Request constructor
    , modifyDBSubnetGroup
    -- ** Request lenses
    , mdbsgDBSubnetGroupDescription
    , mdbsgDBSubnetGroupName
    , mdbsgSubnetIds

    -- * Response
    , ModifyDBSubnetGroupResponse
    -- ** Response constructor
    , modifyDBSubnetGroupResponse
    -- ** Response lenses
    , mdbsgrDBSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data ModifyDBSubnetGroup = ModifyDBSubnetGroup
    { _mdbsgDBSubnetGroupDescription :: Maybe Text
    , _mdbsgDBSubnetGroupName        :: Text
    , _mdbsgSubnetIds                :: List "SubnetIdentifier" Text
    } deriving (Eq, Ord, Show)

-- | 'ModifyDBSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbsgDBSubnetGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'mdbsgDBSubnetGroupName' @::@ 'Text'
--
-- * 'mdbsgSubnetIds' @::@ ['Text']
--
modifyDBSubnetGroup :: Text -- ^ 'mdbsgDBSubnetGroupName'
                    -> ModifyDBSubnetGroup
modifyDBSubnetGroup p1 = ModifyDBSubnetGroup
    { _mdbsgDBSubnetGroupName        = p1
    , _mdbsgDBSubnetGroupDescription = Nothing
    , _mdbsgSubnetIds                = mempty
    }

-- | The description for the DB subnet group.
mdbsgDBSubnetGroupDescription :: Lens' ModifyDBSubnetGroup (Maybe Text)
mdbsgDBSubnetGroupDescription =
    lens _mdbsgDBSubnetGroupDescription
        (\s a -> s { _mdbsgDBSubnetGroupDescription = a })

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens. Must not be "Default".
--
-- Example: 'mySubnetgroup'
mdbsgDBSubnetGroupName :: Lens' ModifyDBSubnetGroup Text
mdbsgDBSubnetGroupName =
    lens _mdbsgDBSubnetGroupName (\s a -> s { _mdbsgDBSubnetGroupName = a })

-- | The EC2 subnet IDs for the DB subnet group.
mdbsgSubnetIds :: Lens' ModifyDBSubnetGroup [Text]
mdbsgSubnetIds = lens _mdbsgSubnetIds (\s a -> s { _mdbsgSubnetIds = a }) . _List

newtype ModifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse
    { _mdbsgrDBSubnetGroup :: Maybe DBSubnetGroup
    } deriving (Eq, Show)

-- | 'ModifyDBSubnetGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbsgrDBSubnetGroup' @::@ 'Maybe' 'DBSubnetGroup'
--
modifyDBSubnetGroupResponse :: ModifyDBSubnetGroupResponse
modifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse
    { _mdbsgrDBSubnetGroup = Nothing
    }

mdbsgrDBSubnetGroup :: Lens' ModifyDBSubnetGroupResponse (Maybe DBSubnetGroup)
mdbsgrDBSubnetGroup =
    lens _mdbsgrDBSubnetGroup (\s a -> s { _mdbsgrDBSubnetGroup = a })

instance ToPath ModifyDBSubnetGroup where
    toPath = const "/"

instance ToQuery ModifyDBSubnetGroup where
    toQuery ModifyDBSubnetGroup{..} = mconcat
        [ "DBSubnetGroupDescription" =? _mdbsgDBSubnetGroupDescription
        , "DBSubnetGroupName"        =? _mdbsgDBSubnetGroupName
        , "SubnetIds"                =? _mdbsgSubnetIds
        ]

instance ToHeaders ModifyDBSubnetGroup

instance AWSRequest ModifyDBSubnetGroup where
    type Sv ModifyDBSubnetGroup = RDS
    type Rs ModifyDBSubnetGroup = ModifyDBSubnetGroupResponse

    request  = post "ModifyDBSubnetGroup"
    response = xmlResponse

instance FromXML ModifyDBSubnetGroupResponse where
    parseXML = withElement "ModifyDBSubnetGroupResult" $ \x -> ModifyDBSubnetGroupResponse
        <$> x .@? "DBSubnetGroup"
