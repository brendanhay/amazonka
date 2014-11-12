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

-- Module      : Network.AWS.RDS.CreateOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new option group. You can create up to 20 option groups.
module Network.AWS.RDS.CreateOptionGroup
    (
    -- * Request
      CreateOptionGroupMessage
    -- ** Request constructor
    , createOptionGroupMessage
    -- ** Request lenses
    , cogmEngineName
    , cogmMajorEngineVersion
    , cogmOptionGroupDescription
    , cogmOptionGroupName
    , cogmTags

    -- * Response
    , CreateOptionGroupResult
    -- ** Response constructor
    , createOptionGroupResult
    -- ** Response lenses
    , cogr1OptionGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CreateOptionGroupMessage = CreateOptionGroupMessage
    { _cogmEngineName             :: Text
    , _cogmMajorEngineVersion     :: Text
    , _cogmOptionGroupDescription :: Text
    , _cogmOptionGroupName        :: Text
    , _cogmTags                   :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateOptionGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogmEngineName' @::@ 'Text'
--
-- * 'cogmMajorEngineVersion' @::@ 'Text'
--
-- * 'cogmOptionGroupDescription' @::@ 'Text'
--
-- * 'cogmOptionGroupName' @::@ 'Text'
--
-- * 'cogmTags' @::@ ['Tag']
--
createOptionGroupMessage :: Text -- ^ 'cogmOptionGroupName'
                         -> Text -- ^ 'cogmEngineName'
                         -> Text -- ^ 'cogmMajorEngineVersion'
                         -> Text -- ^ 'cogmOptionGroupDescription'
                         -> CreateOptionGroupMessage
createOptionGroupMessage p1 p2 p3 p4 = CreateOptionGroupMessage
    { _cogmOptionGroupName        = p1
    , _cogmEngineName             = p2
    , _cogmMajorEngineVersion     = p3
    , _cogmOptionGroupDescription = p4
    , _cogmTags                   = mempty
    }

-- | Specifies the name of the engine that this option group should be
-- associated with.
cogmEngineName :: Lens' CreateOptionGroupMessage Text
cogmEngineName = lens _cogmEngineName (\s a -> s { _cogmEngineName = a })

-- | Specifies the major version of the engine that this option group should
-- be associated with.
cogmMajorEngineVersion :: Lens' CreateOptionGroupMessage Text
cogmMajorEngineVersion =
    lens _cogmMajorEngineVersion (\s a -> s { _cogmMajorEngineVersion = a })

-- | The description of the option group.
cogmOptionGroupDescription :: Lens' CreateOptionGroupMessage Text
cogmOptionGroupDescription =
    lens _cogmOptionGroupDescription
        (\s a -> s { _cogmOptionGroupDescription = a })

-- | Specifies the name of the option group to be created. Constraints: Must
-- be 1 to 255 alphanumeric characters or hyphens First character must be a
-- letter Cannot end with a hyphen or contain two consecutive hyphens
-- Example: myoptiongroup.
cogmOptionGroupName :: Lens' CreateOptionGroupMessage Text
cogmOptionGroupName =
    lens _cogmOptionGroupName (\s a -> s { _cogmOptionGroupName = a })

cogmTags :: Lens' CreateOptionGroupMessage [Tag]
cogmTags = lens _cogmTags (\s a -> s { _cogmTags = a })

instance ToQuery CreateOptionGroupMessage

instance ToPath CreateOptionGroupMessage where
    toPath = const "/"

newtype CreateOptionGroupResult = CreateOptionGroupResult
    { _cogr1OptionGroup :: Maybe OptionGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateOptionGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cogr1OptionGroup' @::@ 'Maybe' 'OptionGroup'
--
createOptionGroupResult :: CreateOptionGroupResult
createOptionGroupResult = CreateOptionGroupResult
    { _cogr1OptionGroup = Nothing
    }

cogr1OptionGroup :: Lens' CreateOptionGroupResult (Maybe OptionGroup)
cogr1OptionGroup = lens _cogr1OptionGroup (\s a -> s { _cogr1OptionGroup = a })

instance FromXML CreateOptionGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateOptionGroupResult"

instance AWSRequest CreateOptionGroupMessage where
    type Sv CreateOptionGroupMessage = RDS
    type Rs CreateOptionGroupMessage = CreateOptionGroupResult

    request  = post "CreateOptionGroup"
    response = xmlResponse $ \h x -> CreateOptionGroupResult
        <$> x %| "OptionGroup"
