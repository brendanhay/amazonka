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
-- Module      : Network.AWS.Glue.Types.ConnectionPasswordEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConnectionPasswordEncryption where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The data structure used by the Data Catalog to encrypt the password as
-- part of @CreateConnection@ or @UpdateConnection@ and store it in the
-- @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable
-- catalog encryption or only password encryption.
--
-- When a @CreationConnection@ request arrives containing a password, the
-- Data Catalog first encrypts the password using your AWS KMS key. It then
-- encrypts the whole connection object again if catalog encryption is also
-- enabled.
--
-- This encryption requires that you set AWS KMS key permissions to enable
-- or restrict access on the password key according to your security
-- requirements. For example, you might want only administrators to have
-- decrypt permission on the password key.
--
-- /See:/ 'newConnectionPasswordEncryption' smart constructor.
data ConnectionPasswordEncryption = ConnectionPasswordEncryption'
  { -- | An AWS KMS key that is used to encrypt the connection password.
    --
    -- If connection password protection is enabled, the caller of
    -- @CreateConnection@ and @UpdateConnection@ needs at least @kms:Encrypt@
    -- permission on the specified AWS KMS key, to encrypt passwords before
    -- storing them in the Data Catalog.
    --
    -- You can set the decrypt permission to enable or restrict access on the
    -- password key according to your security requirements.
    awsKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | When the @ReturnConnectionPasswordEncrypted@ flag is set to \"true\",
    -- passwords remain encrypted in the responses of @GetConnection@ and
    -- @GetConnections@. This encryption takes effect independently from
    -- catalog encryption.
    returnConnectionPasswordEncrypted :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ConnectionPasswordEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsKmsKeyId', 'connectionPasswordEncryption_awsKmsKeyId' - An AWS KMS key that is used to encrypt the connection password.
--
-- If connection password protection is enabled, the caller of
-- @CreateConnection@ and @UpdateConnection@ needs at least @kms:Encrypt@
-- permission on the specified AWS KMS key, to encrypt passwords before
-- storing them in the Data Catalog.
--
-- You can set the decrypt permission to enable or restrict access on the
-- password key according to your security requirements.
--
-- 'returnConnectionPasswordEncrypted', 'connectionPasswordEncryption_returnConnectionPasswordEncrypted' - When the @ReturnConnectionPasswordEncrypted@ flag is set to \"true\",
-- passwords remain encrypted in the responses of @GetConnection@ and
-- @GetConnections@. This encryption takes effect independently from
-- catalog encryption.
newConnectionPasswordEncryption ::
  -- | 'returnConnectionPasswordEncrypted'
  Prelude.Bool ->
  ConnectionPasswordEncryption
newConnectionPasswordEncryption
  pReturnConnectionPasswordEncrypted_ =
    ConnectionPasswordEncryption'
      { awsKmsKeyId =
          Prelude.Nothing,
        returnConnectionPasswordEncrypted =
          pReturnConnectionPasswordEncrypted_
      }

-- | An AWS KMS key that is used to encrypt the connection password.
--
-- If connection password protection is enabled, the caller of
-- @CreateConnection@ and @UpdateConnection@ needs at least @kms:Encrypt@
-- permission on the specified AWS KMS key, to encrypt passwords before
-- storing them in the Data Catalog.
--
-- You can set the decrypt permission to enable or restrict access on the
-- password key according to your security requirements.
connectionPasswordEncryption_awsKmsKeyId :: Lens.Lens' ConnectionPasswordEncryption (Prelude.Maybe Prelude.Text)
connectionPasswordEncryption_awsKmsKeyId = Lens.lens (\ConnectionPasswordEncryption' {awsKmsKeyId} -> awsKmsKeyId) (\s@ConnectionPasswordEncryption' {} a -> s {awsKmsKeyId = a} :: ConnectionPasswordEncryption)

-- | When the @ReturnConnectionPasswordEncrypted@ flag is set to \"true\",
-- passwords remain encrypted in the responses of @GetConnection@ and
-- @GetConnections@. This encryption takes effect independently from
-- catalog encryption.
connectionPasswordEncryption_returnConnectionPasswordEncrypted :: Lens.Lens' ConnectionPasswordEncryption Prelude.Bool
connectionPasswordEncryption_returnConnectionPasswordEncrypted = Lens.lens (\ConnectionPasswordEncryption' {returnConnectionPasswordEncrypted} -> returnConnectionPasswordEncrypted) (\s@ConnectionPasswordEncryption' {} a -> s {returnConnectionPasswordEncrypted = a} :: ConnectionPasswordEncryption)

instance
  Prelude.FromJSON
    ConnectionPasswordEncryption
  where
  parseJSON =
    Prelude.withObject
      "ConnectionPasswordEncryption"
      ( \x ->
          ConnectionPasswordEncryption'
            Prelude.<$> (x Prelude..:? "AwsKmsKeyId")
            Prelude.<*> (x Prelude..: "ReturnConnectionPasswordEncrypted")
      )

instance
  Prelude.Hashable
    ConnectionPasswordEncryption

instance Prelude.NFData ConnectionPasswordEncryption

instance Prelude.ToJSON ConnectionPasswordEncryption where
  toJSON ConnectionPasswordEncryption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AwsKmsKeyId" Prelude..=) Prelude.<$> awsKmsKeyId,
            Prelude.Just
              ( "ReturnConnectionPasswordEncrypted"
                  Prelude..= returnConnectionPasswordEncrypted
              )
          ]
      )
