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
-- Module      : Network.AWS.ServiceCatalog.Types.Principal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.Principal where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.PrincipalType

-- | Information about a principal.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The ARN of the principal (IAM user, role, or group).
    principalARN :: Prelude.Maybe Prelude.Text,
    -- | The principal type. The supported value is @IAM@.
    principalType :: Prelude.Maybe PrincipalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { principalARN = Prelude.Nothing,
      principalType = Prelude.Nothing
    }

-- | The ARN of the principal (IAM user, role, or group).
principal_principalARN :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_principalARN = Lens.lens (\Principal' {principalARN} -> principalARN) (\s@Principal' {} a -> s {principalARN = a} :: Principal)

-- | The principal type. The supported value is @IAM@.
principal_principalType :: Lens.Lens' Principal (Prelude.Maybe PrincipalType)
principal_principalType = Lens.lens (\Principal' {principalType} -> principalType) (\s@Principal' {} a -> s {principalType = a} :: Principal)

instance Prelude.FromJSON Principal where
  parseJSON =
    Prelude.withObject
      "Principal"
      ( \x ->
          Principal'
            Prelude.<$> (x Prelude..:? "PrincipalARN")
            Prelude.<*> (x Prelude..:? "PrincipalType")
      )

instance Prelude.Hashable Principal

instance Prelude.NFData Principal
