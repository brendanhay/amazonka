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
-- Module      : Amazonka.Glue.Types.SecurityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SecurityConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.EncryptionConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Specifies a security configuration.
--
-- /See:/ 'newSecurityConfiguration' smart constructor.
data SecurityConfiguration = SecurityConfiguration'
  { -- | The time at which this security configuration was created.
    createdTimeStamp :: Prelude.Maybe Data.POSIX,
    -- | The encryption configuration associated with this security
    -- configuration.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The name of the security configuration.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimeStamp', 'securityConfiguration_createdTimeStamp' - The time at which this security configuration was created.
--
-- 'encryptionConfiguration', 'securityConfiguration_encryptionConfiguration' - The encryption configuration associated with this security
-- configuration.
--
-- 'name', 'securityConfiguration_name' - The name of the security configuration.
newSecurityConfiguration ::
  SecurityConfiguration
newSecurityConfiguration =
  SecurityConfiguration'
    { createdTimeStamp =
        Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The time at which this security configuration was created.
securityConfiguration_createdTimeStamp :: Lens.Lens' SecurityConfiguration (Prelude.Maybe Prelude.UTCTime)
securityConfiguration_createdTimeStamp = Lens.lens (\SecurityConfiguration' {createdTimeStamp} -> createdTimeStamp) (\s@SecurityConfiguration' {} a -> s {createdTimeStamp = a} :: SecurityConfiguration) Prelude.. Lens.mapping Data._Time

-- | The encryption configuration associated with this security
-- configuration.
securityConfiguration_encryptionConfiguration :: Lens.Lens' SecurityConfiguration (Prelude.Maybe EncryptionConfiguration)
securityConfiguration_encryptionConfiguration = Lens.lens (\SecurityConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@SecurityConfiguration' {} a -> s {encryptionConfiguration = a} :: SecurityConfiguration)

-- | The name of the security configuration.
securityConfiguration_name :: Lens.Lens' SecurityConfiguration (Prelude.Maybe Prelude.Text)
securityConfiguration_name = Lens.lens (\SecurityConfiguration' {name} -> name) (\s@SecurityConfiguration' {} a -> s {name = a} :: SecurityConfiguration)

instance Data.FromJSON SecurityConfiguration where
  parseJSON =
    Data.withObject
      "SecurityConfiguration"
      ( \x ->
          SecurityConfiguration'
            Prelude.<$> (x Data..:? "CreatedTimeStamp")
            Prelude.<*> (x Data..:? "EncryptionConfiguration")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable SecurityConfiguration where
  hashWithSalt _salt SecurityConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimeStamp
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` name

instance Prelude.NFData SecurityConfiguration where
  rnf SecurityConfiguration' {..} =
    Prelude.rnf createdTimeStamp
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf name
