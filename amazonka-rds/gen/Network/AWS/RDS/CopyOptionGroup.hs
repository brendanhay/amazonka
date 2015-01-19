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

-- Module      : Network.AWS.RDS.CopyOptionGroup
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

-- | Copies the specified option group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CopyOptionGroup.html>
module Network.AWS.RDS.CopyOptionGroup
    (
    -- * Request
      CopyOptionGroup
    -- ** Request constructor
    , copyOptionGroup
    -- ** Request lenses
    , cog1SourceOptionGroupIdentifier
    , cog1Tags
    , cog1TargetOptionGroupDescription
    , cog1TargetOptionGroupIdentifier

    -- * Response
    , CopyOptionGroupResponse
    -- ** Response constructor
    , copyOptionGroupResponse
    -- ** Response lenses
    , cogrOptionGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data CopyOptionGroup = CopyOptionGroup
    { _cog1SourceOptionGroupIdentifier  :: Text
    , _cog1Tags                         :: List "member" Tag
    , _cog1TargetOptionGroupDescription :: Text
    , _cog1TargetOptionGroupIdentifier  :: Text
    } deriving (Eq, Show)

-- | 'CopyOptionGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cog1SourceOptionGroupIdentifier' @::@ 'Text'
--
-- * 'cog1Tags' @::@ ['Tag']
--
-- * 'cog1TargetOptionGroupDescription' @::@ 'Text'
--
-- * 'cog1TargetOptionGroupIdentifier' @::@ 'Text'
--
copyOptionGroup :: Text -- ^ 'cog1SourceOptionGroupIdentifier'
                -> Text -- ^ 'cog1TargetOptionGroupIdentifier'
                -> Text -- ^ 'cog1TargetOptionGroupDescription'
                -> CopyOptionGroup
copyOptionGroup p1 p2 p3 = CopyOptionGroup
    { _cog1SourceOptionGroupIdentifier  = p1
    , _cog1TargetOptionGroupIdentifier  = p2
    , _cog1TargetOptionGroupDescription = p3
    , _cog1Tags                         = mempty
    }

-- | The identifier or ARN for the source option group.
--
-- Constraints:
--
-- Must specify a valid option group. If the source option group is in the
-- same region as the copy, specify a valid option group identifier, for example 'my-option-group', or a valid ARN. If the source option group is in a different
-- region than the copy, specify a valid option group ARN, for example 'arn:aws:rds:us-west-2:123456789012:og:special-options'.
cog1SourceOptionGroupIdentifier :: Lens' CopyOptionGroup Text
cog1SourceOptionGroupIdentifier =
    lens _cog1SourceOptionGroupIdentifier
        (\s a -> s { _cog1SourceOptionGroupIdentifier = a })

cog1Tags :: Lens' CopyOptionGroup [Tag]
cog1Tags = lens _cog1Tags (\s a -> s { _cog1Tags = a }) . _List

-- | The description for the copied option group.
cog1TargetOptionGroupDescription :: Lens' CopyOptionGroup Text
cog1TargetOptionGroupDescription =
    lens _cog1TargetOptionGroupDescription
        (\s a -> s { _cog1TargetOptionGroupDescription = a })

-- | The identifier for the copied option group.
--
-- Constraints:
--
-- Cannot be null, empty, or blank Must contain from 1 to 255 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens  Example: 'my-option-group'
cog1TargetOptionGroupIdentifier :: Lens' CopyOptionGroup Text
cog1TargetOptionGroupIdentifier =
    lens _cog1TargetOptionGroupIdentifier
        (\s a -> s { _cog1TargetOptionGroupIdentifier = a })

newtype CopyOptionGroupResponse = CopyOptionGroupResponse
    { _cogrOptionGroup :: Maybe OptionGroup
    } deriving (Eq, Show)

-- | 'CopyOptionGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogrOptionGroup' @::@ 'Maybe' 'OptionGroup'
--
copyOptionGroupResponse :: CopyOptionGroupResponse
copyOptionGroupResponse = CopyOptionGroupResponse
    { _cogrOptionGroup = Nothing
    }

cogrOptionGroup :: Lens' CopyOptionGroupResponse (Maybe OptionGroup)
cogrOptionGroup = lens _cogrOptionGroup (\s a -> s { _cogrOptionGroup = a })

instance ToPath CopyOptionGroup where
    toPath = const "/"

instance ToQuery CopyOptionGroup where
    toQuery CopyOptionGroup{..} = mconcat
        [ "SourceOptionGroupIdentifier"  =? _cog1SourceOptionGroupIdentifier
        , "Tags"                         =? _cog1Tags
        , "TargetOptionGroupDescription" =? _cog1TargetOptionGroupDescription
        , "TargetOptionGroupIdentifier"  =? _cog1TargetOptionGroupIdentifier
        ]

instance ToHeaders CopyOptionGroup

instance AWSRequest CopyOptionGroup where
    type Sv CopyOptionGroup = RDS
    type Rs CopyOptionGroup = CopyOptionGroupResponse

    request  = post "CopyOptionGroup"
    response = xmlResponse

instance FromXML CopyOptionGroupResponse where
    parseXML = withElement "CopyOptionGroupResult" $ \x -> CopyOptionGroupResponse
        <$> x .@? "OptionGroup"
