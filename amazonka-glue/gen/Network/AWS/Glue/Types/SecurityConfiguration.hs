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
-- Module      : Network.AWS.Glue.Types.SecurityConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SecurityConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.EncryptionConfiguration
import qualified Network.AWS.Lens as Lens

-- | Specifies a security configuration.
--
-- /See:/ 'newSecurityConfiguration' smart constructor.
data SecurityConfiguration = SecurityConfiguration'
  { -- | The encryption configuration associated with this security
    -- configuration.
    encryptionConfiguration :: Core.Maybe EncryptionConfiguration,
    -- | The time at which this security configuration was created.
    createdTimeStamp :: Core.Maybe Core.POSIX,
    -- | The name of the security configuration.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfiguration', 'securityConfiguration_encryptionConfiguration' - The encryption configuration associated with this security
-- configuration.
--
-- 'createdTimeStamp', 'securityConfiguration_createdTimeStamp' - The time at which this security configuration was created.
--
-- 'name', 'securityConfiguration_name' - The name of the security configuration.
newSecurityConfiguration ::
  SecurityConfiguration
newSecurityConfiguration =
  SecurityConfiguration'
    { encryptionConfiguration =
        Core.Nothing,
      createdTimeStamp = Core.Nothing,
      name = Core.Nothing
    }

-- | The encryption configuration associated with this security
-- configuration.
securityConfiguration_encryptionConfiguration :: Lens.Lens' SecurityConfiguration (Core.Maybe EncryptionConfiguration)
securityConfiguration_encryptionConfiguration = Lens.lens (\SecurityConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@SecurityConfiguration' {} a -> s {encryptionConfiguration = a} :: SecurityConfiguration)

-- | The time at which this security configuration was created.
securityConfiguration_createdTimeStamp :: Lens.Lens' SecurityConfiguration (Core.Maybe Core.UTCTime)
securityConfiguration_createdTimeStamp = Lens.lens (\SecurityConfiguration' {createdTimeStamp} -> createdTimeStamp) (\s@SecurityConfiguration' {} a -> s {createdTimeStamp = a} :: SecurityConfiguration) Core.. Lens.mapping Core._Time

-- | The name of the security configuration.
securityConfiguration_name :: Lens.Lens' SecurityConfiguration (Core.Maybe Core.Text)
securityConfiguration_name = Lens.lens (\SecurityConfiguration' {name} -> name) (\s@SecurityConfiguration' {} a -> s {name = a} :: SecurityConfiguration)

instance Core.FromJSON SecurityConfiguration where
  parseJSON =
    Core.withObject
      "SecurityConfiguration"
      ( \x ->
          SecurityConfiguration'
            Core.<$> (x Core..:? "EncryptionConfiguration")
            Core.<*> (x Core..:? "CreatedTimeStamp")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable SecurityConfiguration

instance Core.NFData SecurityConfiguration
