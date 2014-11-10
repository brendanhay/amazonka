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

-- Module      : Network.AWS.RDS.CreateDBSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB security group. DB security groups control access to a DB
-- instance.
module Network.AWS.RDS.CreateDBSecurityGroup
    (
    -- * Request
      CreateDBSecurityGroupMessage
    -- ** Request constructor
    , createDBSecurityGroup
    -- ** Request lenses
    , cdbsgm1DBSecurityGroupDescription
    , cdbsgm1DBSecurityGroupName
    , cdbsgm1Tags

    -- * Response
    , CreateDBSecurityGroupResult
    -- ** Response constructor
    , createDBSecurityGroupResponse
    -- ** Response lenses
    , cdbsgrDBSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CreateDBSecurityGroupMessage = CreateDBSecurityGroupMessage
    { _cdbsgm1DBSecurityGroupDescription :: Text
    , _cdbsgm1DBSecurityGroupName        :: Text
    , _cdbsgm1Tags                       :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateDBSecurityGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsgm1DBSecurityGroupDescription' @::@ 'Text'
--
-- * 'cdbsgm1DBSecurityGroupName' @::@ 'Text'
--
-- * 'cdbsgm1Tags' @::@ ['Tag']
--
createDBSecurityGroup :: Text -- ^ 'cdbsgm1DBSecurityGroupName'
                      -> Text -- ^ 'cdbsgm1DBSecurityGroupDescription'
                      -> CreateDBSecurityGroupMessage
createDBSecurityGroup p1 p2 = CreateDBSecurityGroupMessage
    { _cdbsgm1DBSecurityGroupName        = p1
    , _cdbsgm1DBSecurityGroupDescription = p2
    , _cdbsgm1Tags                       = mempty
    }

-- | The description for the DB security group.
cdbsgm1DBSecurityGroupDescription :: Lens' CreateDBSecurityGroupMessage Text
cdbsgm1DBSecurityGroupDescription =
    lens _cdbsgm1DBSecurityGroupDescription
        (\s a -> s { _cdbsgm1DBSecurityGroupDescription = a })

-- | The name for the DB security group. This value is stored as a lowercase
-- string. Constraints: Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Must not be "Default" May not contain spaces Example:
-- mysecuritygroup.
cdbsgm1DBSecurityGroupName :: Lens' CreateDBSecurityGroupMessage Text
cdbsgm1DBSecurityGroupName =
    lens _cdbsgm1DBSecurityGroupName
        (\s a -> s { _cdbsgm1DBSecurityGroupName = a })

cdbsgm1Tags :: Lens' CreateDBSecurityGroupMessage [Tag]
cdbsgm1Tags = lens _cdbsgm1Tags (\s a -> s { _cdbsgm1Tags = a })

instance ToPath CreateDBSecurityGroupMessage where
    toPath = const "/"

instance ToQuery CreateDBSecurityGroupMessage

newtype CreateDBSecurityGroupResult = CreateDBSecurityGroupResult
    { _cdbsgrDBSecurityGroup :: Maybe DBSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateDBSecurityGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsgrDBSecurityGroup' @::@ 'Maybe' 'DBSecurityGroup'
--
createDBSecurityGroupResponse :: CreateDBSecurityGroupResult
createDBSecurityGroupResponse = CreateDBSecurityGroupResult
    { _cdbsgrDBSecurityGroup = Nothing
    }

cdbsgrDBSecurityGroup :: Lens' CreateDBSecurityGroupResult (Maybe DBSecurityGroup)
cdbsgrDBSecurityGroup =
    lens _cdbsgrDBSecurityGroup (\s a -> s { _cdbsgrDBSecurityGroup = a })

instance AWSRequest CreateDBSecurityGroupMessage where
    type Sv CreateDBSecurityGroupMessage = RDS
    type Rs CreateDBSecurityGroupMessage = CreateDBSecurityGroupResult

    request  = post "CreateDBSecurityGroup"
    response = xmlResponse $ \h x -> CreateDBSecurityGroupResult
        <$> x %| "DBSecurityGroup"
