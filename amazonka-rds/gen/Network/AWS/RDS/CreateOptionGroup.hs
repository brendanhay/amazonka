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

-- Module      : Network.AWS.RDS.CreateOptionGroup
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

-- | Creates a new option group. You can create up to 20 option groups.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateOptionGroup.html>
module Network.AWS.RDS.CreateOptionGroup
    (
    -- * Request
      CreateOptionGroup
    -- ** Request constructor
    , createOptionGroup
    -- ** Request lenses
    , cogEngineName
    , cogMajorEngineVersion
    , cogOptionGroupDescription
    , cogOptionGroupName
    , cogTags

    -- * Response
    , CreateOptionGroupResponse
    -- ** Response constructor
    , createOptionGroupResponse
    -- ** Response lenses
    , cogr1OptionGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data CreateOptionGroup = CreateOptionGroup
    { _cogEngineName             :: Text
    , _cogMajorEngineVersion     :: Text
    , _cogOptionGroupDescription :: Text
    , _cogOptionGroupName        :: Text
    , _cogTags                   :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'CreateOptionGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogEngineName' @::@ 'Text'
--
-- * 'cogMajorEngineVersion' @::@ 'Text'
--
-- * 'cogOptionGroupDescription' @::@ 'Text'
--
-- * 'cogOptionGroupName' @::@ 'Text'
--
-- * 'cogTags' @::@ ['Tag']
--
createOptionGroup :: Text -- ^ 'cogOptionGroupName'
                  -> Text -- ^ 'cogEngineName'
                  -> Text -- ^ 'cogMajorEngineVersion'
                  -> Text -- ^ 'cogOptionGroupDescription'
                  -> CreateOptionGroup
createOptionGroup p1 p2 p3 p4 = CreateOptionGroup
    { _cogOptionGroupName        = p1
    , _cogEngineName             = p2
    , _cogMajorEngineVersion     = p3
    , _cogOptionGroupDescription = p4
    , _cogTags                   = mempty
    }

-- | Specifies the name of the engine that this option group should be associated
-- with.
cogEngineName :: Lens' CreateOptionGroup Text
cogEngineName = lens _cogEngineName (\s a -> s { _cogEngineName = a })

-- | Specifies the major version of the engine that this option group should be
-- associated with.
cogMajorEngineVersion :: Lens' CreateOptionGroup Text
cogMajorEngineVersion =
    lens _cogMajorEngineVersion (\s a -> s { _cogMajorEngineVersion = a })

-- | The description of the option group.
cogOptionGroupDescription :: Lens' CreateOptionGroup Text
cogOptionGroupDescription =
    lens _cogOptionGroupDescription
        (\s a -> s { _cogOptionGroupDescription = a })

-- | Specifies the name of the option group to be created.
--
-- Constraints:
--
-- Must be 1 to 255 alphanumeric characters or hyphens First character must be
-- a letter Cannot end with a hyphen or contain two consecutive hyphens  Example:
-- 'myoptiongroup'
cogOptionGroupName :: Lens' CreateOptionGroup Text
cogOptionGroupName =
    lens _cogOptionGroupName (\s a -> s { _cogOptionGroupName = a })

cogTags :: Lens' CreateOptionGroup [Tag]
cogTags = lens _cogTags (\s a -> s { _cogTags = a }) . _List

newtype CreateOptionGroupResponse = CreateOptionGroupResponse
    { _cogr1OptionGroup :: Maybe OptionGroup
    } deriving (Eq, Read, Show)

-- | 'CreateOptionGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogr1OptionGroup' @::@ 'Maybe' 'OptionGroup'
--
createOptionGroupResponse :: CreateOptionGroupResponse
createOptionGroupResponse = CreateOptionGroupResponse
    { _cogr1OptionGroup = Nothing
    }

cogr1OptionGroup :: Lens' CreateOptionGroupResponse (Maybe OptionGroup)
cogr1OptionGroup = lens _cogr1OptionGroup (\s a -> s { _cogr1OptionGroup = a })

instance ToPath CreateOptionGroup where
    toPath = const "/"

instance ToQuery CreateOptionGroup where
    toQuery CreateOptionGroup{..} = mconcat
        [ "EngineName"             =? _cogEngineName
        , "MajorEngineVersion"     =? _cogMajorEngineVersion
        , "OptionGroupDescription" =? _cogOptionGroupDescription
        , "OptionGroupName"        =? _cogOptionGroupName
        , "Tags"                   =? _cogTags
        ]

instance ToHeaders CreateOptionGroup

instance AWSRequest CreateOptionGroup where
    type Sv CreateOptionGroup = RDS
    type Rs CreateOptionGroup = CreateOptionGroupResponse

    request  = post "CreateOptionGroup"
    response = xmlResponse

instance FromXML CreateOptionGroupResponse where
    parseXML = withElement "CreateOptionGroupResult" $ \x -> CreateOptionGroupResponse
        <$> x .@? "OptionGroup"
