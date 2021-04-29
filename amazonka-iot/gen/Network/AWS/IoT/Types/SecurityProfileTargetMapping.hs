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
-- Module      : Network.AWS.IoT.Types.SecurityProfileTargetMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileTargetMapping where

import Network.AWS.IoT.Types.SecurityProfileIdentifier
import Network.AWS.IoT.Types.SecurityProfileTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a security profile and the target associated with it.
--
-- /See:/ 'newSecurityProfileTargetMapping' smart constructor.
data SecurityProfileTargetMapping = SecurityProfileTargetMapping'
  { -- | Information about the target (thing group) associated with the security
    -- profile.
    target :: Prelude.Maybe SecurityProfileTarget,
    -- | Information that identifies the security profile.
    securityProfileIdentifier :: Prelude.Maybe SecurityProfileIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecurityProfileTargetMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'target', 'securityProfileTargetMapping_target' - Information about the target (thing group) associated with the security
-- profile.
--
-- 'securityProfileIdentifier', 'securityProfileTargetMapping_securityProfileIdentifier' - Information that identifies the security profile.
newSecurityProfileTargetMapping ::
  SecurityProfileTargetMapping
newSecurityProfileTargetMapping =
  SecurityProfileTargetMapping'
    { target =
        Prelude.Nothing,
      securityProfileIdentifier = Prelude.Nothing
    }

-- | Information about the target (thing group) associated with the security
-- profile.
securityProfileTargetMapping_target :: Lens.Lens' SecurityProfileTargetMapping (Prelude.Maybe SecurityProfileTarget)
securityProfileTargetMapping_target = Lens.lens (\SecurityProfileTargetMapping' {target} -> target) (\s@SecurityProfileTargetMapping' {} a -> s {target = a} :: SecurityProfileTargetMapping)

-- | Information that identifies the security profile.
securityProfileTargetMapping_securityProfileIdentifier :: Lens.Lens' SecurityProfileTargetMapping (Prelude.Maybe SecurityProfileIdentifier)
securityProfileTargetMapping_securityProfileIdentifier = Lens.lens (\SecurityProfileTargetMapping' {securityProfileIdentifier} -> securityProfileIdentifier) (\s@SecurityProfileTargetMapping' {} a -> s {securityProfileIdentifier = a} :: SecurityProfileTargetMapping)

instance
  Prelude.FromJSON
    SecurityProfileTargetMapping
  where
  parseJSON =
    Prelude.withObject
      "SecurityProfileTargetMapping"
      ( \x ->
          SecurityProfileTargetMapping'
            Prelude.<$> (x Prelude..:? "target")
            Prelude.<*> (x Prelude..:? "securityProfileIdentifier")
      )

instance
  Prelude.Hashable
    SecurityProfileTargetMapping

instance Prelude.NFData SecurityProfileTargetMapping
