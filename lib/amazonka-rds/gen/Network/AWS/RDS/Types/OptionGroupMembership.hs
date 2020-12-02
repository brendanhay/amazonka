{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroupMembership where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information on the option groups the DB instance is a member of.
--
--
--
-- /See:/ 'optionGroupMembership' smart constructor.
data OptionGroupMembership = OptionGroupMembership'
  { _ogmStatus ::
      !(Maybe Text),
    _ogmOptionGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OptionGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogmStatus' - The status of the DB instance's option group membership. Valid values are: @in-sync@ , @pending-apply@ , @pending-removal@ , @pending-maintenance-apply@ , @pending-maintenance-removal@ , @applying@ , @removing@ , and @failed@ .
--
-- * 'ogmOptionGroupName' - The name of the option group that the instance belongs to.
optionGroupMembership ::
  OptionGroupMembership
optionGroupMembership =
  OptionGroupMembership'
    { _ogmStatus = Nothing,
      _ogmOptionGroupName = Nothing
    }

-- | The status of the DB instance's option group membership. Valid values are: @in-sync@ , @pending-apply@ , @pending-removal@ , @pending-maintenance-apply@ , @pending-maintenance-removal@ , @applying@ , @removing@ , and @failed@ .
ogmStatus :: Lens' OptionGroupMembership (Maybe Text)
ogmStatus = lens _ogmStatus (\s a -> s {_ogmStatus = a})

-- | The name of the option group that the instance belongs to.
ogmOptionGroupName :: Lens' OptionGroupMembership (Maybe Text)
ogmOptionGroupName = lens _ogmOptionGroupName (\s a -> s {_ogmOptionGroupName = a})

instance FromXML OptionGroupMembership where
  parseXML x =
    OptionGroupMembership'
      <$> (x .@? "Status") <*> (x .@? "OptionGroupName")

instance Hashable OptionGroupMembership

instance NFData OptionGroupMembership
