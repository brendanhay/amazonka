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
-- Module      : Network.AWS.GameLift.Types.AwsCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.AwsCredentials where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Temporary access credentials used for uploading game build files to
-- Amazon GameLift. They are valid for a limited time. If they expire
-- before you upload your game build, get a new set by calling
-- RequestUploadCredentials.
--
-- /See:/ 'newAwsCredentials' smart constructor.
data AwsCredentials = AwsCredentials'
  { -- | Temporary secret key allowing access to the Amazon GameLift S3 account.
    secretAccessKey :: Prelude.Maybe Prelude.Text,
    -- | Temporary key allowing access to the Amazon GameLift S3 account.
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | Token used to associate a specific build ID with the files uploaded
    -- using these credentials.
    sessionToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AwsCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretAccessKey', 'awsCredentials_secretAccessKey' - Temporary secret key allowing access to the Amazon GameLift S3 account.
--
-- 'accessKeyId', 'awsCredentials_accessKeyId' - Temporary key allowing access to the Amazon GameLift S3 account.
--
-- 'sessionToken', 'awsCredentials_sessionToken' - Token used to associate a specific build ID with the files uploaded
-- using these credentials.
newAwsCredentials ::
  AwsCredentials
newAwsCredentials =
  AwsCredentials'
    { secretAccessKey = Prelude.Nothing,
      accessKeyId = Prelude.Nothing,
      sessionToken = Prelude.Nothing
    }

-- | Temporary secret key allowing access to the Amazon GameLift S3 account.
awsCredentials_secretAccessKey :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Text)
awsCredentials_secretAccessKey = Lens.lens (\AwsCredentials' {secretAccessKey} -> secretAccessKey) (\s@AwsCredentials' {} a -> s {secretAccessKey = a} :: AwsCredentials)

-- | Temporary key allowing access to the Amazon GameLift S3 account.
awsCredentials_accessKeyId :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Text)
awsCredentials_accessKeyId = Lens.lens (\AwsCredentials' {accessKeyId} -> accessKeyId) (\s@AwsCredentials' {} a -> s {accessKeyId = a} :: AwsCredentials)

-- | Token used to associate a specific build ID with the files uploaded
-- using these credentials.
awsCredentials_sessionToken :: Lens.Lens' AwsCredentials (Prelude.Maybe Prelude.Text)
awsCredentials_sessionToken = Lens.lens (\AwsCredentials' {sessionToken} -> sessionToken) (\s@AwsCredentials' {} a -> s {sessionToken = a} :: AwsCredentials)

instance Prelude.FromJSON AwsCredentials where
  parseJSON =
    Prelude.withObject
      "AwsCredentials"
      ( \x ->
          AwsCredentials'
            Prelude.<$> (x Prelude..:? "SecretAccessKey")
            Prelude.<*> (x Prelude..:? "AccessKeyId")
            Prelude.<*> (x Prelude..:? "SessionToken")
      )

instance Prelude.Hashable AwsCredentials

instance Prelude.NFData AwsCredentials
