{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.CreateOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new option group. You can create up to 20 option groups.
-- https://rds.amazonaws.com/ ?Action=CreateOptionGroup
-- &OptionGroupName=myoptiongroup &EngineName=oracle-se1
-- &MajorEngineVersion=11.2 &OptionGroupDescription=Test option group 11.2
-- myoptiongroup oracle-se1 Test option group
-- b2416a8a-84c9-11e1-a264-0b23c28bc344.
module Network.AWS.RDS.V2013_09_09.CreateOptionGroup
    (
    -- * Request
      CreateOptionGroup
    -- ** Request constructor
    , createOptionGroup
    -- ** Request lenses
    , cogmOptionGroupName
    , cogmEngineName
    , cogmMajorEngineVersion
    , cogmOptionGroupDescription
    , cogmTags

    -- * Response
    , CreateOptionGroupResponse
    -- ** Response lenses
    , ogwOptionGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateOptionGroup' request.
createOptionGroup :: Text -- ^ 'cogmOptionGroupName'
                  -> Text -- ^ 'cogmEngineName'
                  -> Text -- ^ 'cogmMajorEngineVersion'
                  -> Text -- ^ 'cogmOptionGroupDescription'
                  -> CreateOptionGroup
createOptionGroup p1 p2 p3 p4 = CreateOptionGroup
    { _cogmOptionGroupName = p1
    , _cogmEngineName = p2
    , _cogmMajorEngineVersion = p3
    , _cogmOptionGroupDescription = p4
    , _cogmTags = mempty
    }

data CreateOptionGroup = CreateOptionGroup
    { _cogmOptionGroupName :: Text
      -- ^ Specifies the name of the option group to be created.
      -- Constraints: Must be 1 to 255 alphanumeric characters or hyphens
      -- First character must be a letter Cannot end with a hyphen or
      -- contain two consecutive hyphens Example: myoptiongroup.
    , _cogmEngineName :: Text
      -- ^ Specifies the name of the engine that this option group should be
      -- associated with.
    , _cogmMajorEngineVersion :: Text
      -- ^ Specifies the major version of the engine that this option group
      -- should be associated with.
    , _cogmOptionGroupDescription :: Text
      -- ^ The description of the option group.
    , _cogmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Show, Generic)

-- | Specifies the name of the option group to be created. Constraints: Must be
-- 1 to 255 alphanumeric characters or hyphens First character must be a
-- letter Cannot end with a hyphen or contain two consecutive hyphens Example:
-- myoptiongroup.
cogmOptionGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateOptionGroup
    -> f CreateOptionGroup
cogmOptionGroupName f x =
    (\y -> x { _cogmOptionGroupName = y })
       <$> f (_cogmOptionGroupName x)
{-# INLINE cogmOptionGroupName #-}

-- | Specifies the name of the engine that this option group should be
-- associated with.
cogmEngineName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateOptionGroup
    -> f CreateOptionGroup
cogmEngineName f x =
    (\y -> x { _cogmEngineName = y })
       <$> f (_cogmEngineName x)
{-# INLINE cogmEngineName #-}

-- | Specifies the major version of the engine that this option group should be
-- associated with.
cogmMajorEngineVersion
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateOptionGroup
    -> f CreateOptionGroup
cogmMajorEngineVersion f x =
    (\y -> x { _cogmMajorEngineVersion = y })
       <$> f (_cogmMajorEngineVersion x)
{-# INLINE cogmMajorEngineVersion #-}

-- | The description of the option group.
cogmOptionGroupDescription
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateOptionGroup
    -> f CreateOptionGroup
cogmOptionGroupDescription f x =
    (\y -> x { _cogmOptionGroupDescription = y })
       <$> f (_cogmOptionGroupDescription x)
{-# INLINE cogmOptionGroupDescription #-}

-- | A list of tags.
cogmTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> CreateOptionGroup
    -> f CreateOptionGroup
cogmTags f x =
    (\y -> x { _cogmTags = y })
       <$> f (_cogmTags x)
{-# INLINE cogmTags #-}

instance ToQuery CreateOptionGroup where
    toQuery = genericQuery def

data CreateOptionGroupResponse = CreateOptionGroupResponse
    { _ogwOptionGroup :: Maybe OptionGroup
      -- ^ 
    } deriving (Show, Generic)

-- | 
ogwOptionGroup
    :: Functor f
    => (Maybe OptionGroup
    -> f (Maybe OptionGroup))
    -> CreateOptionGroupResponse
    -> f CreateOptionGroupResponse
ogwOptionGroup f x =
    (\y -> x { _ogwOptionGroup = y })
       <$> f (_ogwOptionGroup x)
{-# INLINE ogwOptionGroup #-}

instance FromXML CreateOptionGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateOptionGroup where
    type Sv CreateOptionGroup = RDS
    type Rs CreateOptionGroup = CreateOptionGroupResponse

    request = post "CreateOptionGroup"
    response _ = xmlResponse
