{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.RoleUsageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.RoleUsageType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains details about how a service-linked role is used, if that information is returned by the service.
--
--
-- This data type is used as a response element in the 'GetServiceLinkedRoleDeletionStatus' operation.
--
--
-- /See:/ 'roleUsageType' smart constructor.
data RoleUsageType = RoleUsageType'
  { _rutResources ::
      !(Maybe [Text]),
    _rutRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoleUsageType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rutResources' - The name of the resource that is using the service-linked role.
--
-- * 'rutRegion' - The name of the Region where the service-linked role is being used.
roleUsageType ::
  RoleUsageType
roleUsageType =
  RoleUsageType' {_rutResources = Nothing, _rutRegion = Nothing}

-- | The name of the resource that is using the service-linked role.
rutResources :: Lens' RoleUsageType [Text]
rutResources = lens _rutResources (\s a -> s {_rutResources = a}) . _Default . _Coerce

-- | The name of the Region where the service-linked role is being used.
rutRegion :: Lens' RoleUsageType (Maybe Text)
rutRegion = lens _rutRegion (\s a -> s {_rutRegion = a})

instance FromXML RoleUsageType where
  parseXML x =
    RoleUsageType'
      <$> (x .@? "Resources" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Region")

instance Hashable RoleUsageType

instance NFData RoleUsageType
