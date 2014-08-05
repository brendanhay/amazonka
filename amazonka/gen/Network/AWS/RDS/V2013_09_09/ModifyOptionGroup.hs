{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.RDS.V2013_09_09.ModifyOptionGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyOptionGroup' request.
modifyOptionGroup :: Text -- ^ '_mogmOptionGroupName'
                  -> ModifyOptionGroup
modifyOptionGroup p1 = ModifyOptionGroup
    { _mogmOptionGroupName = p1
    , _mogmApplyImmediately = Nothing
    , _mogmOptionsToInclude = mempty
    , _mogmOptionsToRemove = mempty
    }

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

makeLenses ''ModifyOptionGroup

instance ToQuery ModifyOptionGroup where
    toQuery = genericToQuery def

data ModifyOptionGroupResponse = ModifyOptionGroupResponse
    { _ogwOptionGroup :: Maybe OptionGroup
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''ModifyOptionGroupResponse

instance AWSRequest ModifyOptionGroup where
    type Sv ModifyOptionGroup = RDS
    type Rs ModifyOptionGroup = ModifyOptionGroupResponse

    request = post "ModifyOptionGroup"
    response _ = cursorResponse $ \hs xml ->
        pure ModifyOptionGroupResponse
            <*> xml %|? "OptionGroup"
