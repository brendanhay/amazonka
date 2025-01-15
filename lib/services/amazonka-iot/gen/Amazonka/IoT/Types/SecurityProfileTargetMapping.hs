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
-- Module      : Amazonka.IoT.Types.SecurityProfileTargetMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.SecurityProfileTargetMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.SecurityProfileIdentifier
import Amazonka.IoT.Types.SecurityProfileTarget
import qualified Amazonka.Prelude as Prelude

-- | Information about a security profile and the target associated with it.
--
-- /See:/ 'newSecurityProfileTargetMapping' smart constructor.
data SecurityProfileTargetMapping = SecurityProfileTargetMapping'
  { -- | Information that identifies the security profile.
    securityProfileIdentifier :: Prelude.Maybe SecurityProfileIdentifier,
    -- | Information about the target (thing group) associated with the security
    -- profile.
    target :: Prelude.Maybe SecurityProfileTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityProfileTargetMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileIdentifier', 'securityProfileTargetMapping_securityProfileIdentifier' - Information that identifies the security profile.
--
-- 'target', 'securityProfileTargetMapping_target' - Information about the target (thing group) associated with the security
-- profile.
newSecurityProfileTargetMapping ::
  SecurityProfileTargetMapping
newSecurityProfileTargetMapping =
  SecurityProfileTargetMapping'
    { securityProfileIdentifier =
        Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | Information that identifies the security profile.
securityProfileTargetMapping_securityProfileIdentifier :: Lens.Lens' SecurityProfileTargetMapping (Prelude.Maybe SecurityProfileIdentifier)
securityProfileTargetMapping_securityProfileIdentifier = Lens.lens (\SecurityProfileTargetMapping' {securityProfileIdentifier} -> securityProfileIdentifier) (\s@SecurityProfileTargetMapping' {} a -> s {securityProfileIdentifier = a} :: SecurityProfileTargetMapping)

-- | Information about the target (thing group) associated with the security
-- profile.
securityProfileTargetMapping_target :: Lens.Lens' SecurityProfileTargetMapping (Prelude.Maybe SecurityProfileTarget)
securityProfileTargetMapping_target = Lens.lens (\SecurityProfileTargetMapping' {target} -> target) (\s@SecurityProfileTargetMapping' {} a -> s {target = a} :: SecurityProfileTargetMapping)

instance Data.FromJSON SecurityProfileTargetMapping where
  parseJSON =
    Data.withObject
      "SecurityProfileTargetMapping"
      ( \x ->
          SecurityProfileTargetMapping'
            Prelude.<$> (x Data..:? "securityProfileIdentifier")
            Prelude.<*> (x Data..:? "target")
      )

instance
  Prelude.Hashable
    SecurityProfileTargetMapping
  where
  hashWithSalt _salt SecurityProfileTargetMapping' {..} =
    _salt
      `Prelude.hashWithSalt` securityProfileIdentifier
      `Prelude.hashWithSalt` target

instance Prelude.NFData SecurityProfileTargetMapping where
  rnf SecurityProfileTargetMapping' {..} =
    Prelude.rnf securityProfileIdentifier `Prelude.seq`
      Prelude.rnf target
