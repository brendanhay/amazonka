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

-- Module      : Network.AWS.RDS.CopyOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Copies the specified Option Group.
module Network.AWS.RDS.CopyOptionGroup
    (
    -- * Request
      CopyOptionGroupMessage
    -- ** Request constructor
    , copyOptionGroupMessage
    -- ** Request lenses
    , cogm1SourceOptionGroupIdentifier
    , cogm1Tags
    , cogm1TargetOptionGroupDescription
    , cogm1TargetOptionGroupIdentifier

    -- * Response
    , CopyOptionGroupResult
    -- ** Response constructor
    , copyOptionGroupResult
    -- ** Response lenses
    , cogrOptionGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CopyOptionGroupMessage = CopyOptionGroupMessage
    { _cogm1SourceOptionGroupIdentifier  :: Text
    , _cogm1Tags                         :: [Tag]
    , _cogm1TargetOptionGroupDescription :: Text
    , _cogm1TargetOptionGroupIdentifier  :: Text
    } deriving (Eq, Show, Generic)

-- | 'CopyOptionGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogm1SourceOptionGroupIdentifier' @::@ 'Text'
--
-- * 'cogm1Tags' @::@ ['Tag']
--
-- * 'cogm1TargetOptionGroupDescription' @::@ 'Text'
--
-- * 'cogm1TargetOptionGroupIdentifier' @::@ 'Text'
--
copyOptionGroupMessage :: Text -- ^ 'cogm1SourceOptionGroupIdentifier'
                       -> Text -- ^ 'cogm1TargetOptionGroupIdentifier'
                       -> Text -- ^ 'cogm1TargetOptionGroupDescription'
                       -> CopyOptionGroupMessage
copyOptionGroupMessage p1 p2 p3 = CopyOptionGroupMessage
    { _cogm1SourceOptionGroupIdentifier  = p1
    , _cogm1TargetOptionGroupIdentifier  = p2
    , _cogm1TargetOptionGroupDescription = p3
    , _cogm1Tags                         = mempty
    }

-- | The identifier or ARN for the source Option Group. Constraints: Must
-- specify a valid Option Group. If the source Option Group is in the same
-- region as the copy, specify a valid Option Group identifier, or a valid
-- ARN. If the source Option Group is in a different region than the copy,
-- specify a valid Option group ARN. Example: my-option-group Example:
-- arn:aws:rds:us-west-2:123456789012:og:special-options.
cogm1SourceOptionGroupIdentifier :: Lens' CopyOptionGroupMessage Text
cogm1SourceOptionGroupIdentifier =
    lens _cogm1SourceOptionGroupIdentifier
        (\s a -> s { _cogm1SourceOptionGroupIdentifier = a })

cogm1Tags :: Lens' CopyOptionGroupMessage [Tag]
cogm1Tags = lens _cogm1Tags (\s a -> s { _cogm1Tags = a })

-- | The description for the copied Option Group.
cogm1TargetOptionGroupDescription :: Lens' CopyOptionGroupMessage Text
cogm1TargetOptionGroupDescription =
    lens _cogm1TargetOptionGroupDescription
        (\s a -> s { _cogm1TargetOptionGroupDescription = a })

-- | The identifier for the copied Option Group. Constraints: Cannot be null,
-- empty, or blank Must contain from 1 to 255 alphanumeric characters or
-- hyphens First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens Example: my-option-group.
cogm1TargetOptionGroupIdentifier :: Lens' CopyOptionGroupMessage Text
cogm1TargetOptionGroupIdentifier =
    lens _cogm1TargetOptionGroupIdentifier
        (\s a -> s { _cogm1TargetOptionGroupIdentifier = a })

instance ToQuery CopyOptionGroupMessage

instance ToPath CopyOptionGroupMessage where
    toPath = const "/"

newtype CopyOptionGroupResult = CopyOptionGroupResult
    { _cogrOptionGroup :: Maybe OptionGroup
    } deriving (Eq, Show, Generic)

-- | 'CopyOptionGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogrOptionGroup' @::@ 'Maybe' 'OptionGroup'
--
copyOptionGroupResult :: CopyOptionGroupResult
copyOptionGroupResult = CopyOptionGroupResult
    { _cogrOptionGroup = Nothing
    }

cogrOptionGroup :: Lens' CopyOptionGroupResult (Maybe OptionGroup)
cogrOptionGroup = lens _cogrOptionGroup (\s a -> s { _cogrOptionGroup = a })

instance FromXML CopyOptionGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyOptionGroupResult"

instance AWSRequest CopyOptionGroupMessage where
    type Sv CopyOptionGroupMessage = RDS
    type Rs CopyOptionGroupMessage = CopyOptionGroupResult

    request  = post "CopyOptionGroup"
    response = xmlResponse $ \h x -> CopyOptionGroupResult
        <$> x %| "OptionGroup"
