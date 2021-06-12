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
-- Module      : Network.AWS.ServiceCatalog.Types.Principal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.Principal where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.PrincipalType

-- | Information about a principal.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The ARN of the principal (IAM user, role, or group).
    principalARN :: Core.Maybe Core.Text,
    -- | The principal type. The supported value is @IAM@.
    principalType :: Core.Maybe PrincipalType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Principal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalARN', 'principal_principalARN' - The ARN of the principal (IAM user, role, or group).
--
-- 'principalType', 'principal_principalType' - The principal type. The supported value is @IAM@.
newPrincipal ::
  Principal
newPrincipal =
  Principal'
    { principalARN = Core.Nothing,
      principalType = Core.Nothing
    }

-- | The ARN of the principal (IAM user, role, or group).
principal_principalARN :: Lens.Lens' Principal (Core.Maybe Core.Text)
principal_principalARN = Lens.lens (\Principal' {principalARN} -> principalARN) (\s@Principal' {} a -> s {principalARN = a} :: Principal)

-- | The principal type. The supported value is @IAM@.
principal_principalType :: Lens.Lens' Principal (Core.Maybe PrincipalType)
principal_principalType = Lens.lens (\Principal' {principalType} -> principalType) (\s@Principal' {} a -> s {principalType = a} :: Principal)

instance Core.FromJSON Principal where
  parseJSON =
    Core.withObject
      "Principal"
      ( \x ->
          Principal'
            Core.<$> (x Core..:? "PrincipalARN")
            Core.<*> (x Core..:? "PrincipalType")
      )

instance Core.Hashable Principal

instance Core.NFData Principal
