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
-- Module      : Network.AWS.EC2.Types.AllowedPrincipal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllowedPrincipal where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PrincipalType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a principal.
--
-- /See:/ 'newAllowedPrincipal' smart constructor.
data AllowedPrincipal = AllowedPrincipal'
  { -- | The Amazon Resource Name (ARN) of the principal.
    principal :: Prelude.Maybe Prelude.Text,
    -- | The type of principal.
    principalType :: Prelude.Maybe PrincipalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AllowedPrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'allowedPrincipal_principal' - The Amazon Resource Name (ARN) of the principal.
--
-- 'principalType', 'allowedPrincipal_principalType' - The type of principal.
newAllowedPrincipal ::
  AllowedPrincipal
newAllowedPrincipal =
  AllowedPrincipal'
    { principal = Prelude.Nothing,
      principalType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the principal.
allowedPrincipal_principal :: Lens.Lens' AllowedPrincipal (Prelude.Maybe Prelude.Text)
allowedPrincipal_principal = Lens.lens (\AllowedPrincipal' {principal} -> principal) (\s@AllowedPrincipal' {} a -> s {principal = a} :: AllowedPrincipal)

-- | The type of principal.
allowedPrincipal_principalType :: Lens.Lens' AllowedPrincipal (Prelude.Maybe PrincipalType)
allowedPrincipal_principalType = Lens.lens (\AllowedPrincipal' {principalType} -> principalType) (\s@AllowedPrincipal' {} a -> s {principalType = a} :: AllowedPrincipal)

instance Prelude.FromXML AllowedPrincipal where
  parseXML x =
    AllowedPrincipal'
      Prelude.<$> (x Prelude..@? "principal")
      Prelude.<*> (x Prelude..@? "principalType")

instance Prelude.Hashable AllowedPrincipal

instance Prelude.NFData AllowedPrincipal
