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
-- Module      : Amazonka.CodePipeline.Types.AWSSessionCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.AWSSessionCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an AWS session credentials object. These credentials are
-- temporary credentials that are issued by AWS Secure Token Service (STS).
-- They can be used to access input and output artifacts in the S3 bucket
-- used to store artifact for the pipeline in AWS CodePipeline.
--
-- /See:/ 'newAWSSessionCredentials' smart constructor.
data AWSSessionCredentials = AWSSessionCredentials'
  { -- | The access key for the session.
    accessKeyId :: Data.Sensitive Prelude.Text,
    -- | The secret access key for the session.
    secretAccessKey :: Data.Sensitive Prelude.Text,
    -- | The token for the session.
    sessionToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AWSSessionCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'aWSSessionCredentials_accessKeyId' - The access key for the session.
--
-- 'secretAccessKey', 'aWSSessionCredentials_secretAccessKey' - The secret access key for the session.
--
-- 'sessionToken', 'aWSSessionCredentials_sessionToken' - The token for the session.
newAWSSessionCredentials ::
  -- | 'accessKeyId'
  Prelude.Text ->
  -- | 'secretAccessKey'
  Prelude.Text ->
  -- | 'sessionToken'
  Prelude.Text ->
  AWSSessionCredentials
newAWSSessionCredentials
  pAccessKeyId_
  pSecretAccessKey_
  pSessionToken_ =
    AWSSessionCredentials'
      { accessKeyId =
          Data._Sensitive Lens.# pAccessKeyId_,
        secretAccessKey =
          Data._Sensitive Lens.# pSecretAccessKey_,
        sessionToken = Data._Sensitive Lens.# pSessionToken_
      }

-- | The access key for the session.
aWSSessionCredentials_accessKeyId :: Lens.Lens' AWSSessionCredentials Prelude.Text
aWSSessionCredentials_accessKeyId = Lens.lens (\AWSSessionCredentials' {accessKeyId} -> accessKeyId) (\s@AWSSessionCredentials' {} a -> s {accessKeyId = a} :: AWSSessionCredentials) Prelude.. Data._Sensitive

-- | The secret access key for the session.
aWSSessionCredentials_secretAccessKey :: Lens.Lens' AWSSessionCredentials Prelude.Text
aWSSessionCredentials_secretAccessKey = Lens.lens (\AWSSessionCredentials' {secretAccessKey} -> secretAccessKey) (\s@AWSSessionCredentials' {} a -> s {secretAccessKey = a} :: AWSSessionCredentials) Prelude.. Data._Sensitive

-- | The token for the session.
aWSSessionCredentials_sessionToken :: Lens.Lens' AWSSessionCredentials Prelude.Text
aWSSessionCredentials_sessionToken = Lens.lens (\AWSSessionCredentials' {sessionToken} -> sessionToken) (\s@AWSSessionCredentials' {} a -> s {sessionToken = a} :: AWSSessionCredentials) Prelude.. Data._Sensitive

instance Data.FromJSON AWSSessionCredentials where
  parseJSON =
    Data.withObject
      "AWSSessionCredentials"
      ( \x ->
          AWSSessionCredentials'
            Prelude.<$> (x Data..: "accessKeyId")
            Prelude.<*> (x Data..: "secretAccessKey")
            Prelude.<*> (x Data..: "sessionToken")
      )

instance Prelude.Hashable AWSSessionCredentials where
  hashWithSalt _salt AWSSessionCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` secretAccessKey
      `Prelude.hashWithSalt` sessionToken

instance Prelude.NFData AWSSessionCredentials where
  rnf AWSSessionCredentials' {..} =
    Prelude.rnf accessKeyId `Prelude.seq`
      Prelude.rnf secretAccessKey `Prelude.seq`
        Prelude.rnf sessionToken
