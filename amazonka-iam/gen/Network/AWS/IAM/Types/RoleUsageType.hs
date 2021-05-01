{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.RoleUsageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.RoleUsageType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains details about how a service-linked role is used,
-- if that information is returned by the service.
--
-- This data type is used as a response element in the
-- GetServiceLinkedRoleDeletionStatus operation.
--
-- /See:/ 'newRoleUsageType' smart constructor.
data RoleUsageType = RoleUsageType'
  { -- | The name of the resource that is using the service-linked role.
    resources :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Region where the service-linked role is being used.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RoleUsageType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resources', 'roleUsageType_resources' - The name of the resource that is using the service-linked role.
--
-- 'region', 'roleUsageType_region' - The name of the Region where the service-linked role is being used.
newRoleUsageType ::
  RoleUsageType
newRoleUsageType =
  RoleUsageType'
    { resources = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The name of the resource that is using the service-linked role.
roleUsageType_resources :: Lens.Lens' RoleUsageType (Prelude.Maybe [Prelude.Text])
roleUsageType_resources = Lens.lens (\RoleUsageType' {resources} -> resources) (\s@RoleUsageType' {} a -> s {resources = a} :: RoleUsageType) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the Region where the service-linked role is being used.
roleUsageType_region :: Lens.Lens' RoleUsageType (Prelude.Maybe Prelude.Text)
roleUsageType_region = Lens.lens (\RoleUsageType' {region} -> region) (\s@RoleUsageType' {} a -> s {region = a} :: RoleUsageType)

instance Prelude.FromXML RoleUsageType where
  parseXML x =
    RoleUsageType'
      Prelude.<$> ( x Prelude..@? "Resources" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "Region")

instance Prelude.Hashable RoleUsageType

instance Prelude.NFData RoleUsageType
