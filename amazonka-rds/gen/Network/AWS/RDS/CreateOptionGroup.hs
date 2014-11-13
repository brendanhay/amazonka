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
    , _cogTags                   :: [Tag]
    } deriving (Eq, Show, Generic)

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

-- | Specifies the name of the engine that this option group should be
-- associated with.
cogEngineName :: Lens' CreateOptionGroup Text
cogEngineName = lens _cogEngineName (\s a -> s { _cogEngineName = a })

-- | Specifies the major version of the engine that this option group should
-- be associated with.
cogMajorEngineVersion :: Lens' CreateOptionGroup Text
cogMajorEngineVersion =
    lens _cogMajorEngineVersion (\s a -> s { _cogMajorEngineVersion = a })

-- | The description of the option group.
cogOptionGroupDescription :: Lens' CreateOptionGroup Text
cogOptionGroupDescription =
    lens _cogOptionGroupDescription
        (\s a -> s { _cogOptionGroupDescription = a })

-- | Specifies the name of the option group to be created. Constraints: Must
-- be 1 to 255 alphanumeric characters or hyphens First character must be a
-- letter Cannot end with a hyphen or contain two consecutive hyphens
-- Example: myoptiongroup.
cogOptionGroupName :: Lens' CreateOptionGroup Text
cogOptionGroupName =
    lens _cogOptionGroupName (\s a -> s { _cogOptionGroupName = a })

cogTags :: Lens' CreateOptionGroup [Tag]
cogTags = lens _cogTags (\s a -> s { _cogTags = a })

instance ToQuery CreateOptionGroup

instance ToPath CreateOptionGroup where
    toPath = const "/"

newtype CreateOptionGroupResponse = CreateOptionGroupResponse
    { _cogr1OptionGroup :: Maybe OptionGroup
    } deriving (Eq, Show, Generic)

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

instance AWSRequest CreateOptionGroup where
    type Sv CreateOptionGroup = RDS
    type Rs CreateOptionGroup = CreateOptionGroupResponse

    request  = post "CreateOptionGroup"
    response = xmlResponse $ \h x -> CreateOptionGroupResponse
        <$> x %| "OptionGroup"
