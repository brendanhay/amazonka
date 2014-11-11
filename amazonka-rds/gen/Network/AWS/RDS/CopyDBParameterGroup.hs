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
      CopyDBParameterGroupMessage
    -- ** Request constructor
    , copyDBParameterGroupMessage
    -- ** Request lenses
    , cdbpgm1SourceDBParameterGroupIdentifier
    , cdbpgm1Tags
    , cdbpgm1TargetDBParameterGroupDescription
    , cdbpgm1TargetDBParameterGroupIdentifier

    -- * Response
    , CopyDBParameterGroupResult
    -- ** Response constructor
    , copyDBParameterGroupResult
    -- ** Response lenses
    , cdbpgrDBParameterGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CopyDBParameterGroupMessage = CopyDBParameterGroupMessage
    { _cdbpgm1SourceDBParameterGroupIdentifier  :: Text
    , _cdbpgm1Tags                              :: [Tag]
    , _cdbpgm1TargetDBParameterGroupDescription :: Text
    , _cdbpgm1TargetDBParameterGroupIdentifier  :: Text
    } deriving (Eq, Show, Generic)

-- | 'CopyDBParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbpgm1SourceDBParameterGroupIdentifier' @::@ 'Text'
--
-- * 'cdbpgm1Tags' @::@ ['Tag']
--
-- * 'cdbpgm1TargetDBParameterGroupDescription' @::@ 'Text'
--
-- * 'cdbpgm1TargetDBParameterGroupIdentifier' @::@ 'Text'
--
copyDBParameterGroupMessage :: Text -- ^ 'cdbpgm1SourceDBParameterGroupIdentifier'
                            -> Text -- ^ 'cdbpgm1TargetDBParameterGroupIdentifier'
                            -> Text -- ^ 'cdbpgm1TargetDBParameterGroupDescription'
                            -> CopyDBParameterGroupMessage
copyDBParameterGroupMessage p1 p2 p3 = CopyDBParameterGroupMessage
    { _cdbpgm1SourceDBParameterGroupIdentifier  = p1
    , _cdbpgm1TargetDBParameterGroupIdentifier  = p2
    , _cdbpgm1TargetDBParameterGroupDescription = p3
    , _cdbpgm1Tags                              = mempty
    }

-- | The identifier or ARN for the source DB Parameter Group. Constraints:
-- Must specify a valid DB Parameter Group. If the source DB Parameter Group
-- is in the same region as the copy, specify a valid DB Parameter Group
-- identifier, or a valid ARN. If the source DB Parameter Group is in a
-- different region than the copy, specify a valid DB parameter group ARN.
-- Example: my-db-param-group Example:
-- arn:aws:rds:us-west-2:123456789012:pg:special-parameters.
cdbpgm1SourceDBParameterGroupIdentifier :: Lens' CopyDBParameterGroupMessage Text
cdbpgm1SourceDBParameterGroupIdentifier =
    lens _cdbpgm1SourceDBParameterGroupIdentifier
        (\s a -> s { _cdbpgm1SourceDBParameterGroupIdentifier = a })

cdbpgm1Tags :: Lens' CopyDBParameterGroupMessage [Tag]
cdbpgm1Tags = lens _cdbpgm1Tags (\s a -> s { _cdbpgm1Tags = a })

-- | The description for the copied DB Parameter Group.
cdbpgm1TargetDBParameterGroupDescription :: Lens' CopyDBParameterGroupMessage Text
cdbpgm1TargetDBParameterGroupDescription =
    lens _cdbpgm1TargetDBParameterGroupDescription
        (\s a -> s { _cdbpgm1TargetDBParameterGroupDescription = a })

-- | The identifier for the copied DB Parameter Group. Constraints: Cannot be
-- null, empty, or blank Must contain from 1 to 255 alphanumeric characters
-- or hyphens First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens Example: my-db-parameter-group.
cdbpgm1TargetDBParameterGroupIdentifier :: Lens' CopyDBParameterGroupMessage Text
cdbpgm1TargetDBParameterGroupIdentifier =
    lens _cdbpgm1TargetDBParameterGroupIdentifier
        (\s a -> s { _cdbpgm1TargetDBParameterGroupIdentifier = a })
instance ToQuery CopyDBParameterGroupMessage

instance ToPath CopyDBParameterGroupMessage where
    toPath = const "/"

newtype CopyDBParameterGroupResult = CopyDBParameterGroupResult
    { _cdbpgrDBParameterGroup :: Maybe DBParameterGroup
    } deriving (Eq, Show, Generic)

-- | 'CopyDBParameterGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbpgrDBParameterGroup' @::@ 'Maybe' 'DBParameterGroup'
--
copyDBParameterGroupResult :: CopyDBParameterGroupResult
copyDBParameterGroupResult = CopyDBParameterGroupResult
    { _cdbpgrDBParameterGroup = Nothing
    }

cdbpgrDBParameterGroup :: Lens' CopyDBParameterGroupResult (Maybe DBParameterGroup)
cdbpgrDBParameterGroup =
    lens _cdbpgrDBParameterGroup (\s a -> s { _cdbpgrDBParameterGroup = a })
instance FromXML CopyDBParameterGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyDBParameterGroupResult"

instance AWSRequest CopyDBParameterGroupMessage where
    type Sv CopyDBParameterGroupMessage = RDS
    type Rs CopyDBParameterGroupMessage = CopyDBParameterGroupResult

    request  = post "CopyDBParameterGroup"
    response = xmlResponse $ \h x -> CopyDBParameterGroupResult
        <$> x %| "DBParameterGroup"
