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
-- Module      : Amazonka.GameLift.Types.AwsCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.AwsCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Temporary access credentials used for uploading game build files to
-- Amazon GameLift. They are valid for a limited time. If they expire
-- before you upload your game build, get a new set by calling
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_RequestUploadCredentials.html RequestUploadCredentials>.
--
-- /See:/ 'newAwsCredentials' smart constructor.
data AwsCredentials = AwsCredentials'
  { -- | Temporary key allowing access to the Amazon GameLift S3 account.
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | Temporary secret key allowing access to the Amazon GameLift S3 account.
    secretAccessKey :: Prelude.Maybe Prelude.Text,
    -- | Token used to associate a specific build ID with the files uploaded
    -- using these credentials.
    sessionToken :: Prelude.Maybe Prelude.Text
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
-- 'accessKeyId', 'awsCredentials_accessKeyId' - Temporary key allowing access to the Amazon GameLift S3 account.
--
-- 'secretAccessKey', 'awsCredentials_secretAccessKey' - Temporary secret key allowing access to the Amazon GameLift S3 account.
--
-- 'sessionToken', 'awsCredentials_sessionToken' - Token used to associate a specific build ID with the files uploaded
-- using these credentials.
newAwsCredentials ::
  AwsCredentials
newAwsCredentials =
  AwsCredentials'
    { accessKeyId = Prelude.Nothing,
      secretAccessKey = Prelude.Nothing,
      sessionToken = Prelude.Nothing
    }

-- | Temporary key allowing access to the Amazon GameLift S3 account.
awsCredentials_accessKeyId :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Text)
awsCredentials_accessKeyId = Lens.lens (\AwsCredentials' {accessKeyId} -> accessKeyId) (\s@AwsCredentials' {} a -> s {accessKeyId = a} :: AwsCredentials)

-- | Temporary secret key allowing access to the Amazon GameLift S3 account.
awsCredentials_secretAccessKey :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Text)
awsCredentials_secretAccessKey = Lens.lens (\AwsCredentials' {secretAccessKey} -> secretAccessKey) (\s@AwsCredentials' {} a -> s {secretAccessKey = a} :: AwsCredentials)

-- | Token used to associate a specific build ID with the files uploaded
-- using these credentials.
awsCredentials_sessionToken :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Text)
awsCredentials_sessionToken = Lens.lens (\AwsCredentials' {sessionToken} -> sessionToken) (\s@AwsCredentials' {} a -> s {sessionToken = a} :: AwsCredentials)

instance Data.FromJSON AwsCredentials where
  parseJSON =
    Data.withObject
      "AwsCredentials"
      ( \x ->
          AwsCredentials'
            Prelude.<$> (x Data..:? "AccessKeyId")
            Prelude.<*> (x Data..:? "SecretAccessKey")
            Prelude.<*> (x Data..:? "SessionToken")
      )

instance Prelude.Hashable AwsCredentials where
  hashWithSalt _salt AwsCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` secretAccessKey
      `Prelude.hashWithSalt` sessionToken

instance Prelude.NFData AwsCredentials where
  rnf AwsCredentials' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf secretAccessKey
      `Prelude.seq` Prelude.rnf sessionToken
