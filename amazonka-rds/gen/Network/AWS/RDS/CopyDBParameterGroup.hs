{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CopyDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Copies the specified DBParameterGroup.
module Network.AWS.RDS.CopyDBParameterGroup
    (
    -- * Request
      CopyDBParameterGroup
    -- ** Request constructor
    , copyDBParameterGroup
    -- ** Request lenses
    , cdbpgSourceDBParameterGroupIdentifier
    , cdbpgTags
    , cdbpgTargetDBParameterGroupDescription
    , cdbpgTargetDBParameterGroupIdentifier

    -- * Response
    , CopyDBParameterGroupResponse
    -- ** Response constructor
    , copyDBParameterGroupResponse
    -- ** Response lenses
    , cdbpgr1DBParameterGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data CopyDBParameterGroup = CopyDBParameterGroup
    { _cdbpgSourceDBParameterGroupIdentifier  :: Text
    , _cdbpgTags                              :: [Tag]
    , _cdbpgTargetDBParameterGroupDescription :: Text
    , _cdbpgTargetDBParameterGroupIdentifier  :: Text
    } deriving (Eq, Show, Generic)

-- | 'CopyDBParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbpgSourceDBParameterGroupIdentifier' @::@ 'Text'
--
-- * 'cdbpgTags' @::@ ['Tag']
--
-- * 'cdbpgTargetDBParameterGroupDescription' @::@ 'Text'
--
-- * 'cdbpgTargetDBParameterGroupIdentifier' @::@ 'Text'
--
copyDBParameterGroup :: Text -- ^ 'cdbpgSourceDBParameterGroupIdentifier'
                     -> Text -- ^ 'cdbpgTargetDBParameterGroupIdentifier'
                     -> Text -- ^ 'cdbpgTargetDBParameterGroupDescription'
                     -> CopyDBParameterGroup
copyDBParameterGroup p1 p2 p3 = CopyDBParameterGroup
    { _cdbpgSourceDBParameterGroupIdentifier  = p1
    , _cdbpgTargetDBParameterGroupIdentifier  = p2
    , _cdbpgTargetDBParameterGroupDescription = p3
    , _cdbpgTags                              = mempty
    }

-- | The identifier or ARN for the source DB Parameter Group. Constraints:
-- Must specify a valid DB Parameter Group. If the source DB Parameter Group
-- is in the same region as the copy, specify a valid DB Parameter Group
-- identifier, or a valid ARN. If the source DB Parameter Group is in a
-- different region than the copy, specify a valid DB parameter group ARN.
-- Example: my-db-param-group Example:
-- arn:aws:rds:us-west-2:123456789012:pg:special-parameters.
cdbpgSourceDBParameterGroupIdentifier :: Lens' CopyDBParameterGroup Text
cdbpgSourceDBParameterGroupIdentifier =
    lens _cdbpgSourceDBParameterGroupIdentifier
        (\s a -> s { _cdbpgSourceDBParameterGroupIdentifier = a })

cdbpgTags :: Lens' CopyDBParameterGroup [Tag]
cdbpgTags = lens _cdbpgTags (\s a -> s { _cdbpgTags = a })

-- | The description for the copied DB Parameter Group.
cdbpgTargetDBParameterGroupDescription :: Lens' CopyDBParameterGroup Text
cdbpgTargetDBParameterGroupDescription =
    lens _cdbpgTargetDBParameterGroupDescription
        (\s a -> s { _cdbpgTargetDBParameterGroupDescription = a })

-- | The identifier for the copied DB Parameter Group. Constraints: Cannot be
-- null, empty, or blank Must contain from 1 to 255 alphanumeric characters
-- or hyphens First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens Example: my-db-parameter-group.
cdbpgTargetDBParameterGroupIdentifier :: Lens' CopyDBParameterGroup Text
cdbpgTargetDBParameterGroupIdentifier =
    lens _cdbpgTargetDBParameterGroupIdentifier
        (\s a -> s { _cdbpgTargetDBParameterGroupIdentifier = a })

newtype CopyDBParameterGroupResponse = CopyDBParameterGroupResponse
    { _cdbpgr1DBParameterGroup :: Maybe DBParameterGroup
    } deriving (Eq, Show, Generic)

-- | 'CopyDBParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbpgr1DBParameterGroup' @::@ 'Maybe' 'DBParameterGroup'
--
copyDBParameterGroupResponse :: CopyDBParameterGroupResponse
copyDBParameterGroupResponse = CopyDBParameterGroupResponse
    { _cdbpgr1DBParameterGroup = Nothing
    }

cdbpgr1DBParameterGroup :: Lens' CopyDBParameterGroupResponse (Maybe DBParameterGroup)
cdbpgr1DBParameterGroup =
    lens _cdbpgr1DBParameterGroup (\s a -> s { _cdbpgr1DBParameterGroup = a })

instance AWSRequest CopyDBParameterGroup where
    type Sv CopyDBParameterGroup = RDS
    type Rs CopyDBParameterGroup = CopyDBParameterGroupResponse

    request  = post "CopyDBParameterGroup"
    response = xmlResponse

instance FromXML CopyDBParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyDBParameterGroupResponse"

instance ToPath CopyDBParameterGroup where
    toPath = const "/"

instance ToHeaders CopyDBParameterGroup

instance ToQuery CopyDBParameterGroup
