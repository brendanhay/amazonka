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
-- Module      : Amazonka.GameLift.Types.InstanceCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.InstanceCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Set of credentials required to remotely access a fleet instance.
--
-- /See:/ 'newInstanceCredentials' smart constructor.
data InstanceCredentials = InstanceCredentials'
  { -- | Secret string. For Windows instances, the secret is a password for use
    -- with Windows Remote Desktop. For Linux instances, it is a private key
    -- (which must be saved as a @.pem@ file) for use with SSH.
    secret :: Prelude.Maybe Prelude.Text,
    -- | User login string.
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secret', 'instanceCredentials_secret' - Secret string. For Windows instances, the secret is a password for use
-- with Windows Remote Desktop. For Linux instances, it is a private key
-- (which must be saved as a @.pem@ file) for use with SSH.
--
-- 'userName', 'instanceCredentials_userName' - User login string.
newInstanceCredentials ::
  InstanceCredentials
newInstanceCredentials =
  InstanceCredentials'
    { secret = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | Secret string. For Windows instances, the secret is a password for use
-- with Windows Remote Desktop. For Linux instances, it is a private key
-- (which must be saved as a @.pem@ file) for use with SSH.
instanceCredentials_secret :: Lens.Lens' InstanceCredentials (Prelude.Maybe Prelude.Text)
instanceCredentials_secret = Lens.lens (\InstanceCredentials' {secret} -> secret) (\s@InstanceCredentials' {} a -> s {secret = a} :: InstanceCredentials)

-- | User login string.
instanceCredentials_userName :: Lens.Lens' InstanceCredentials (Prelude.Maybe Prelude.Text)
instanceCredentials_userName = Lens.lens (\InstanceCredentials' {userName} -> userName) (\s@InstanceCredentials' {} a -> s {userName = a} :: InstanceCredentials)

instance Data.FromJSON InstanceCredentials where
  parseJSON =
    Data.withObject
      "InstanceCredentials"
      ( \x ->
          InstanceCredentials'
            Prelude.<$> (x Data..:? "Secret")
            Prelude.<*> (x Data..:? "UserName")
      )

instance Prelude.Hashable InstanceCredentials where
  hashWithSalt _salt InstanceCredentials' {..} =
    _salt `Prelude.hashWithSalt` secret
      `Prelude.hashWithSalt` userName

instance Prelude.NFData InstanceCredentials where
  rnf InstanceCredentials' {..} =
    Prelude.rnf secret
      `Prelude.seq` Prelude.rnf userName
