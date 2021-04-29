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
-- Module      : Network.AWS.GameLift.Types.InstanceCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.InstanceCredentials where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Set of credentials required to remotely access a fleet instance. Access
-- credentials are requested by calling GetInstanceAccess and returned in
-- an InstanceAccess object.
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
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON InstanceCredentials where
  parseJSON =
    Prelude.withObject
      "InstanceCredentials"
      ( \x ->
          InstanceCredentials'
            Prelude.<$> (x Prelude..:? "Secret")
            Prelude.<*> (x Prelude..:? "UserName")
      )

instance Prelude.Hashable InstanceCredentials

instance Prelude.NFData InstanceCredentials
