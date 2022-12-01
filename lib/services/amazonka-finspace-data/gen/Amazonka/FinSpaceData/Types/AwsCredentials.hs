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
-- Module      : Amazonka.FinSpaceData.Types.AwsCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.AwsCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The credentials required to access the external Dataview from the S3
-- location.
--
-- /See:/ 'newAwsCredentials' smart constructor.
data AwsCredentials = AwsCredentials'
  { -- | The Epoch time when the current credentials expire.
    expiration :: Prelude.Maybe Prelude.Integer,
    -- | The token that users must pass to use the credentials.
    sessionToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The secret access key that can be used to sign requests.
    secretAccessKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The unique identifier for the security credentials.
    accessKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiration', 'awsCredentials_expiration' - The Epoch time when the current credentials expire.
--
-- 'sessionToken', 'awsCredentials_sessionToken' - The token that users must pass to use the credentials.
--
-- 'secretAccessKey', 'awsCredentials_secretAccessKey' - The secret access key that can be used to sign requests.
--
-- 'accessKeyId', 'awsCredentials_accessKeyId' - The unique identifier for the security credentials.
newAwsCredentials ::
  AwsCredentials
newAwsCredentials =
  AwsCredentials'
    { expiration = Prelude.Nothing,
      sessionToken = Prelude.Nothing,
      secretAccessKey = Prelude.Nothing,
      accessKeyId = Prelude.Nothing
    }

-- | The Epoch time when the current credentials expire.
awsCredentials_expiration :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Integer)
awsCredentials_expiration = Lens.lens (\AwsCredentials' {expiration} -> expiration) (\s@AwsCredentials' {} a -> s {expiration = a} :: AwsCredentials)

-- | The token that users must pass to use the credentials.
awsCredentials_sessionToken :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Text)
awsCredentials_sessionToken = Lens.lens (\AwsCredentials' {sessionToken} -> sessionToken) (\s@AwsCredentials' {} a -> s {sessionToken = a} :: AwsCredentials) Prelude.. Lens.mapping Core._Sensitive

-- | The secret access key that can be used to sign requests.
awsCredentials_secretAccessKey :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Text)
awsCredentials_secretAccessKey = Lens.lens (\AwsCredentials' {secretAccessKey} -> secretAccessKey) (\s@AwsCredentials' {} a -> s {secretAccessKey = a} :: AwsCredentials) Prelude.. Lens.mapping Core._Sensitive

-- | The unique identifier for the security credentials.
awsCredentials_accessKeyId :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Text)
awsCredentials_accessKeyId = Lens.lens (\AwsCredentials' {accessKeyId} -> accessKeyId) (\s@AwsCredentials' {} a -> s {accessKeyId = a} :: AwsCredentials)

instance Core.FromJSON AwsCredentials where
  parseJSON =
    Core.withObject
      "AwsCredentials"
      ( \x ->
          AwsCredentials'
            Prelude.<$> (x Core..:? "expiration")
            Prelude.<*> (x Core..:? "sessionToken")
            Prelude.<*> (x Core..:? "secretAccessKey")
            Prelude.<*> (x Core..:? "accessKeyId")
      )

instance Prelude.Hashable AwsCredentials where
  hashWithSalt _salt AwsCredentials' {..} =
    _salt `Prelude.hashWithSalt` expiration
      `Prelude.hashWithSalt` sessionToken
      `Prelude.hashWithSalt` secretAccessKey
      `Prelude.hashWithSalt` accessKeyId

instance Prelude.NFData AwsCredentials where
  rnf AwsCredentials' {..} =
    Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf sessionToken
      `Prelude.seq` Prelude.rnf secretAccessKey
      `Prelude.seq` Prelude.rnf accessKeyId
