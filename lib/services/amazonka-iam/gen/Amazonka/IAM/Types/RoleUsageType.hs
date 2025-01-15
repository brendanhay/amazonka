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
-- Module      : Amazonka.IAM.Types.RoleUsageType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.RoleUsageType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains details about how a service-linked role is used,
-- if that information is returned by the service.
--
-- This data type is used as a response element in the
-- GetServiceLinkedRoleDeletionStatus operation.
--
-- /See:/ 'newRoleUsageType' smart constructor.
data RoleUsageType = RoleUsageType'
  { -- | The name of the Region where the service-linked role is being used.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource that is using the service-linked role.
    resources :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoleUsageType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'roleUsageType_region' - The name of the Region where the service-linked role is being used.
--
-- 'resources', 'roleUsageType_resources' - The name of the resource that is using the service-linked role.
newRoleUsageType ::
  RoleUsageType
newRoleUsageType =
  RoleUsageType'
    { region = Prelude.Nothing,
      resources = Prelude.Nothing
    }

-- | The name of the Region where the service-linked role is being used.
roleUsageType_region :: Lens.Lens' RoleUsageType (Prelude.Maybe Prelude.Text)
roleUsageType_region = Lens.lens (\RoleUsageType' {region} -> region) (\s@RoleUsageType' {} a -> s {region = a} :: RoleUsageType)

-- | The name of the resource that is using the service-linked role.
roleUsageType_resources :: Lens.Lens' RoleUsageType (Prelude.Maybe [Prelude.Text])
roleUsageType_resources = Lens.lens (\RoleUsageType' {resources} -> resources) (\s@RoleUsageType' {} a -> s {resources = a} :: RoleUsageType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML RoleUsageType where
  parseXML x =
    RoleUsageType'
      Prelude.<$> (x Data..@? "Region")
      Prelude.<*> ( x Data..@? "Resources" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable RoleUsageType where
  hashWithSalt _salt RoleUsageType' {..} =
    _salt
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` resources

instance Prelude.NFData RoleUsageType where
  rnf RoleUsageType' {..} =
    Prelude.rnf region `Prelude.seq`
      Prelude.rnf resources
