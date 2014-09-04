{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.ModifyOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an existing option group. https://rds.amazonaws.com/
-- ?Action=ModifyOptionGroup &OptionGroupName=myoptiongroup
-- &OptionsToInclude=OEM &DBSecurityGroupMemberships=default
-- &ApplyImmediately=true myoptiongroup Test option group oracle-se1 11.2 OEM
-- Oracle Enterprise Manager 1158 default ACTIVE
-- ed662948-a57b-11df-9e38-7ffab86c801f https://rds.amazonaws.com/
-- ?Action=ModifyOptionGroup &OptionGroupName=myoptiongroup
-- &OptionsToRemove=OEM &ApplyImmediately=true myoptiongroup Test option group
-- oracle-se1 11.2 ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.RDS.V2013_09_09.ModifyOptionGroup
    (
    -- * Request
      ModifyOptionGroup
    -- ** Request constructor
    , modifyOptionGroup
    -- ** Request lenses
    , mogmOptionGroupName
    , mogmApplyImmediately
    , mogmOptionsToInclude
    , mogmOptionsToRemove

    -- * Response
    , ModifyOptionGroupResponse
    -- ** Response lenses
    , ogxOptionGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyOptionGroup' request.
modifyOptionGroup :: Text -- ^ 'mogmOptionGroupName'
                  -> ModifyOptionGroup
modifyOptionGroup p1 = ModifyOptionGroup
    { _mogmOptionGroupName = p1
    , _mogmApplyImmediately = Nothing
    , _mogmOptionsToInclude = mempty
    , _mogmOptionsToRemove = mempty
    }
{-# INLINE modifyOptionGroup #-}

data ModifyOptionGroup = ModifyOptionGroup
    { _mogmOptionGroupName :: Text
      -- ^ The name of the option group to be modified. cannot be removed
      -- from an option group while DB instances are associated with the
      -- option group. --> Permanent options, such as the TDE option for
      -- Oracle Advanced Security TDE, cannot be removed from an option
      -- group, and that option group cannot be removed from a DB instance
      -- once it is associated with a DB instance.
    , _mogmApplyImmediately :: Maybe Bool
      -- ^ Indicates whether the changes should be applied immediately, or
      -- during the next maintenance window for each instance associated
      -- with the option group.
    , _mogmOptionsToInclude :: [OptionConfiguration]
      -- ^ Options in this list are added to the option group or, if already
      -- present, the specified configuration is used to update the
      -- existing configuration.
    , _mogmOptionsToRemove :: [Text]
      -- ^ Options in this list are removed from the option group.
    } deriving (Show, Generic)

-- | The name of the option group to be modified. cannot be removed from an
-- option group while DB instances are associated with the option group. -->
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE,
-- cannot be removed from an option group, and that option group cannot be
-- removed from a DB instance once it is associated with a DB instance.
mogmOptionGroupName :: Lens' ModifyOptionGroup (Text)
mogmOptionGroupName f x =
    f (_mogmOptionGroupName x)
        <&> \y -> x { _mogmOptionGroupName = y }
{-# INLINE mogmOptionGroupName #-}

-- | Indicates whether the changes should be applied immediately, or during the
-- next maintenance window for each instance associated with the option group.
mogmApplyImmediately :: Lens' ModifyOptionGroup (Maybe Bool)
mogmApplyImmediately f x =
    f (_mogmApplyImmediately x)
        <&> \y -> x { _mogmApplyImmediately = y }
{-# INLINE mogmApplyImmediately #-}

-- | Options in this list are added to the option group or, if already present,
-- the specified configuration is used to update the existing configuration.
mogmOptionsToInclude :: Lens' ModifyOptionGroup ([OptionConfiguration])
mogmOptionsToInclude f x =
    f (_mogmOptionsToInclude x)
        <&> \y -> x { _mogmOptionsToInclude = y }
{-# INLINE mogmOptionsToInclude #-}

-- | Options in this list are removed from the option group.
mogmOptionsToRemove :: Lens' ModifyOptionGroup ([Text])
mogmOptionsToRemove f x =
    f (_mogmOptionsToRemove x)
        <&> \y -> x { _mogmOptionsToRemove = y }
{-# INLINE mogmOptionsToRemove #-}

instance ToQuery ModifyOptionGroup where
    toQuery = genericQuery def

data ModifyOptionGroupResponse = ModifyOptionGroupResponse
    { _ogxOptionGroup :: Maybe OptionGroup
      -- ^ 
    } deriving (Show, Generic)

-- | 
ogxOptionGroup :: Lens' ModifyOptionGroupResponse (Maybe OptionGroup)
ogxOptionGroup f x =
    f (_ogxOptionGroup x)
        <&> \y -> x { _ogxOptionGroup = y }
{-# INLINE ogxOptionGroup #-}

instance FromXML ModifyOptionGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyOptionGroup where
    type Sv ModifyOptionGroup = RDS
    type Rs ModifyOptionGroup = ModifyOptionGroupResponse

    request = post "ModifyOptionGroup"
    response _ = xmlResponse
