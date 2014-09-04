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
    , mkCreateOptionGroupMessage
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateOptionGroup' request.
mkCreateOptionGroupMessage :: Text -- ^ 'cogmOptionGroupName'
                           -> Text -- ^ 'cogmEngineName'
                           -> Text -- ^ 'cogmMajorEngineVersion'
                           -> Text -- ^ 'cogmOptionGroupDescription'
                           -> CreateOptionGroup
mkCreateOptionGroupMessage p1 p2 p3 p4 = CreateOptionGroup
    { _cogmOptionGroupName = p1
    , _cogmEngineName = p2
    , _cogmMajorEngineVersion = p3
    , _cogmOptionGroupDescription = p4
    , _cogmTags = mempty
    }
{-# INLINE mkCreateOptionGroupMessage #-}

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
cogmOptionGroupName :: Lens' CreateOptionGroup (Text)
cogmOptionGroupName = lens _cogmOptionGroupName (\s a -> s { _cogmOptionGroupName = a })
{-# INLINE cogmOptionGroupName #-}

-- | Specifies the name of the engine that this option group should be
-- associated with.
cogmEngineName :: Lens' CreateOptionGroup (Text)
cogmEngineName = lens _cogmEngineName (\s a -> s { _cogmEngineName = a })
{-# INLINE cogmEngineName #-}

-- | Specifies the major version of the engine that this option group should be
-- associated with.
cogmMajorEngineVersion :: Lens' CreateOptionGroup (Text)
cogmMajorEngineVersion = lens _cogmMajorEngineVersion (\s a -> s { _cogmMajorEngineVersion = a })
{-# INLINE cogmMajorEngineVersion #-}

-- | The description of the option group.
cogmOptionGroupDescription :: Lens' CreateOptionGroup (Text)
cogmOptionGroupDescription = lens _cogmOptionGroupDescription (\s a -> s { _cogmOptionGroupDescription = a })
{-# INLINE cogmOptionGroupDescription #-}

-- | A list of tags.
cogmTags :: Lens' CreateOptionGroup ([Tag])
cogmTags = lens _cogmTags (\s a -> s { _cogmTags = a })
{-# INLINE cogmTags #-}

instance ToQuery CreateOptionGroup where
    toQuery = genericQuery def

newtype CreateOptionGroupResponse = CreateOptionGroupResponse
    { _ogwOptionGroup :: Maybe OptionGroup
      -- ^ 
    } deriving (Show, Generic)

-- | 
ogwOptionGroup :: Lens' CreateOptionGroupResponse (Maybe OptionGroup)
ogwOptionGroup = lens _ogwOptionGroup (\s a -> s { _ogwOptionGroup = a })
{-# INLINE ogwOptionGroup #-}

instance FromXML CreateOptionGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateOptionGroup where
    type Sv CreateOptionGroup = RDS
    type Rs CreateOptionGroup = CreateOptionGroupResponse

    request = post "CreateOptionGroup"
    response _ = xmlResponse
