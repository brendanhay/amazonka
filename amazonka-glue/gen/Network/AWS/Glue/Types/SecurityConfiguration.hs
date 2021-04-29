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
-- Module      : Network.AWS.Glue.Types.SecurityConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SecurityConfiguration where

import Network.AWS.Glue.Types.EncryptionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a security configuration.
--
-- /See:/ 'newSecurityConfiguration' smart constructor.
data SecurityConfiguration = SecurityConfiguration'
  { -- | The encryption configuration associated with this security
    -- configuration.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The time at which this security configuration was created.
    createdTimeStamp :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the security configuration.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      createdTimeStamp = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The encryption configuration associated with this security
-- configuration.
securityConfiguration_encryptionConfiguration :: Lens.Lens' SecurityConfiguration (Prelude.Maybe EncryptionConfiguration)
securityConfiguration_encryptionConfiguration = Lens.lens (\SecurityConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@SecurityConfiguration' {} a -> s {encryptionConfiguration = a} :: SecurityConfiguration)

-- | The time at which this security configuration was created.
securityConfiguration_createdTimeStamp :: Lens.Lens' SecurityConfiguration (Prelude.Maybe Prelude.UTCTime)
securityConfiguration_createdTimeStamp = Lens.lens (\SecurityConfiguration' {createdTimeStamp} -> createdTimeStamp) (\s@SecurityConfiguration' {} a -> s {createdTimeStamp = a} :: SecurityConfiguration) Prelude.. Lens.mapping Prelude._Time

-- | The name of the security configuration.
securityConfiguration_name :: Lens.Lens' SecurityConfiguration (Prelude.Maybe Prelude.Text)
securityConfiguration_name = Lens.lens (\SecurityConfiguration' {name} -> name) (\s@SecurityConfiguration' {} a -> s {name = a} :: SecurityConfiguration)

instance Prelude.FromJSON SecurityConfiguration where
  parseJSON =
    Prelude.withObject
      "SecurityConfiguration"
      ( \x ->
          SecurityConfiguration'
            Prelude.<$> (x Prelude..:? "EncryptionConfiguration")
            Prelude.<*> (x Prelude..:? "CreatedTimeStamp")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable SecurityConfiguration

instance Prelude.NFData SecurityConfiguration
