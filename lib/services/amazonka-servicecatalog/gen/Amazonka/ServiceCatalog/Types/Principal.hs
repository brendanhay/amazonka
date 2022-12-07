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
-- Module      : Amazonka.ServiceCatalog.Types.Principal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.Principal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.PrincipalType

-- | Information about a principal.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The ARN of the principal (IAM user, role, or group). This field allows
    -- for an ARN with no @accountID@ if the @PrincipalType@ is an
    -- @IAM_PATTERN@.
    principalARN :: Prelude.Maybe Prelude.Text,
    -- | The principal type. The supported value is @IAM@ if you use a fully
    -- defined ARN, or @IAM_PATTERN@ if you use an ARN with no @accountID@.
    principalType :: Prelude.Maybe PrincipalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Principal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalARN', 'principal_principalARN' - The ARN of the principal (IAM user, role, or group). This field allows
-- for an ARN with no @accountID@ if the @PrincipalType@ is an
-- @IAM_PATTERN@.
--
-- 'principalType', 'principal_principalType' - The principal type. The supported value is @IAM@ if you use a fully
-- defined ARN, or @IAM_PATTERN@ if you use an ARN with no @accountID@.
newPrincipal ::
  Principal
newPrincipal =
  Principal'
    { principalARN = Prelude.Nothing,
      principalType = Prelude.Nothing
    }

-- | The ARN of the principal (IAM user, role, or group). This field allows
-- for an ARN with no @accountID@ if the @PrincipalType@ is an
-- @IAM_PATTERN@.
principal_principalARN :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_principalARN = Lens.lens (\Principal' {principalARN} -> principalARN) (\s@Principal' {} a -> s {principalARN = a} :: Principal)

-- | The principal type. The supported value is @IAM@ if you use a fully
-- defined ARN, or @IAM_PATTERN@ if you use an ARN with no @accountID@.
principal_principalType :: Lens.Lens' Principal (Prelude.Maybe PrincipalType)
principal_principalType = Lens.lens (\Principal' {principalType} -> principalType) (\s@Principal' {} a -> s {principalType = a} :: Principal)

instance Data.FromJSON Principal where
  parseJSON =
    Data.withObject
      "Principal"
      ( \x ->
          Principal'
            Prelude.<$> (x Data..:? "PrincipalARN")
            Prelude.<*> (x Data..:? "PrincipalType")
      )

instance Prelude.Hashable Principal where
  hashWithSalt _salt Principal' {..} =
    _salt `Prelude.hashWithSalt` principalARN
      `Prelude.hashWithSalt` principalType

instance Prelude.NFData Principal where
  rnf Principal' {..} =
    Prelude.rnf principalARN
      `Prelude.seq` Prelude.rnf principalType
