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
-- Module      : Amazonka.EC2.Types.AllowedPrincipal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AllowedPrincipal where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PrincipalType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a principal.
--
-- /See:/ 'newAllowedPrincipal' smart constructor.
data AllowedPrincipal = AllowedPrincipal'
  { -- | The type of principal.
    principalType :: Prelude.Maybe PrincipalType,
    -- | The Amazon Resource Name (ARN) of the principal.
    principal :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowedPrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalType', 'allowedPrincipal_principalType' - The type of principal.
--
-- 'principal', 'allowedPrincipal_principal' - The Amazon Resource Name (ARN) of the principal.
newAllowedPrincipal ::
  AllowedPrincipal
newAllowedPrincipal =
  AllowedPrincipal'
    { principalType = Prelude.Nothing,
      principal = Prelude.Nothing
    }

-- | The type of principal.
allowedPrincipal_principalType :: Lens.Lens' AllowedPrincipal (Prelude.Maybe PrincipalType)
allowedPrincipal_principalType = Lens.lens (\AllowedPrincipal' {principalType} -> principalType) (\s@AllowedPrincipal' {} a -> s {principalType = a} :: AllowedPrincipal)

-- | The Amazon Resource Name (ARN) of the principal.
allowedPrincipal_principal :: Lens.Lens' AllowedPrincipal (Prelude.Maybe Prelude.Text)
allowedPrincipal_principal = Lens.lens (\AllowedPrincipal' {principal} -> principal) (\s@AllowedPrincipal' {} a -> s {principal = a} :: AllowedPrincipal)

instance Core.FromXML AllowedPrincipal where
  parseXML x =
    AllowedPrincipal'
      Prelude.<$> (x Core..@? "principalType")
      Prelude.<*> (x Core..@? "principal")

instance Prelude.Hashable AllowedPrincipal where
  hashWithSalt _salt AllowedPrincipal' {..} =
    _salt `Prelude.hashWithSalt` principalType
      `Prelude.hashWithSalt` principal

instance Prelude.NFData AllowedPrincipal where
  rnf AllowedPrincipal' {..} =
    Prelude.rnf principalType
      `Prelude.seq` Prelude.rnf principal
