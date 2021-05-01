{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.StartStreamEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or updates server-side encryption using an AWS KMS key for a
-- specified stream.
--
-- Starting encryption is an asynchronous operation. Upon receiving the
-- request, Kinesis Data Streams returns immediately and sets the status of
-- the stream to @UPDATING@. After the update is complete, Kinesis Data
-- Streams sets the status of the stream back to @ACTIVE@. Updating or
-- applying encryption normally takes a few seconds to complete, but it can
-- take minutes. You can continue to read and write data to your stream
-- while its status is @UPDATING@. Once the status of the stream is
-- @ACTIVE@, encryption begins for records written to the stream.
--
-- API Limits: You can successfully apply a new AWS KMS key for server-side
-- encryption 25 times in a rolling 24-hour period.
--
-- Note: It can take up to 5 seconds after the stream is in an @ACTIVE@
-- status before all records written to the stream are encrypted. After you
-- enable encryption, you can verify that encryption is applied by
-- inspecting the API response from @PutRecord@ or @PutRecords@.
module Network.AWS.Kinesis.StartStreamEncryption
  ( -- * Creating a Request
    StartStreamEncryption (..),
    newStartStreamEncryption,

    -- * Request Lenses
    startStreamEncryption_streamName,
    startStreamEncryption_encryptionType,
    startStreamEncryption_keyId,

    -- * Destructuring the Response
    StartStreamEncryptionResponse (..),
    newStartStreamEncryptionResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartStreamEncryption' smart constructor.
data StartStreamEncryption = StartStreamEncryption'
  { -- | The name of the stream for which to start encrypting records.
    streamName :: Prelude.Text,
    -- | The encryption type to use. The only valid value is @KMS@.
    encryptionType :: EncryptionType,
    -- | The GUID for the customer-managed AWS KMS key to use for encryption.
    -- This value can be a globally unique identifier, a fully specified Amazon
    -- Resource Name (ARN) to either an alias or a key, or an alias name
    -- prefixed by \"alias\/\".You can also use a master key owned by Kinesis
    -- Data Streams by specifying the alias @aws\/kinesis@.
    --
    -- -   Key ARN example:
    --     @arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012@
    --
    -- -   Alias ARN example:
    --     @arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName@
    --
    -- -   Globally unique key ID example:
    --     @12345678-1234-1234-1234-123456789012@
    --
    -- -   Alias name example: @alias\/MyAliasName@
    --
    -- -   Master key owned by Kinesis Data Streams: @alias\/aws\/kinesis@
    keyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartStreamEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamName', 'startStreamEncryption_streamName' - The name of the stream for which to start encrypting records.
--
-- 'encryptionType', 'startStreamEncryption_encryptionType' - The encryption type to use. The only valid value is @KMS@.
--
-- 'keyId', 'startStreamEncryption_keyId' - The GUID for the customer-managed AWS KMS key to use for encryption.
-- This value can be a globally unique identifier, a fully specified Amazon
-- Resource Name (ARN) to either an alias or a key, or an alias name
-- prefixed by \"alias\/\".You can also use a master key owned by Kinesis
-- Data Streams by specifying the alias @aws\/kinesis@.
--
-- -   Key ARN example:
--     @arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- -   Alias ARN example:
--     @arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName@
--
-- -   Globally unique key ID example:
--     @12345678-1234-1234-1234-123456789012@
--
-- -   Alias name example: @alias\/MyAliasName@
--
-- -   Master key owned by Kinesis Data Streams: @alias\/aws\/kinesis@
newStartStreamEncryption ::
  -- | 'streamName'
  Prelude.Text ->
  -- | 'encryptionType'
  EncryptionType ->
  -- | 'keyId'
  Prelude.Text ->
  StartStreamEncryption
newStartStreamEncryption
  pStreamName_
  pEncryptionType_
  pKeyId_ =
    StartStreamEncryption'
      { streamName = pStreamName_,
        encryptionType = pEncryptionType_,
        keyId = pKeyId_
      }

-- | The name of the stream for which to start encrypting records.
startStreamEncryption_streamName :: Lens.Lens' StartStreamEncryption Prelude.Text
startStreamEncryption_streamName = Lens.lens (\StartStreamEncryption' {streamName} -> streamName) (\s@StartStreamEncryption' {} a -> s {streamName = a} :: StartStreamEncryption)

-- | The encryption type to use. The only valid value is @KMS@.
startStreamEncryption_encryptionType :: Lens.Lens' StartStreamEncryption EncryptionType
startStreamEncryption_encryptionType = Lens.lens (\StartStreamEncryption' {encryptionType} -> encryptionType) (\s@StartStreamEncryption' {} a -> s {encryptionType = a} :: StartStreamEncryption)

-- | The GUID for the customer-managed AWS KMS key to use for encryption.
-- This value can be a globally unique identifier, a fully specified Amazon
-- Resource Name (ARN) to either an alias or a key, or an alias name
-- prefixed by \"alias\/\".You can also use a master key owned by Kinesis
-- Data Streams by specifying the alias @aws\/kinesis@.
--
-- -   Key ARN example:
--     @arn:aws:kms:us-east-1:123456789012:key\/12345678-1234-1234-1234-123456789012@
--
-- -   Alias ARN example:
--     @arn:aws:kms:us-east-1:123456789012:alias\/MyAliasName@
--
-- -   Globally unique key ID example:
--     @12345678-1234-1234-1234-123456789012@
--
-- -   Alias name example: @alias\/MyAliasName@
--
-- -   Master key owned by Kinesis Data Streams: @alias\/aws\/kinesis@
startStreamEncryption_keyId :: Lens.Lens' StartStreamEncryption Prelude.Text
startStreamEncryption_keyId = Lens.lens (\StartStreamEncryption' {keyId} -> keyId) (\s@StartStreamEncryption' {} a -> s {keyId = a} :: StartStreamEncryption)

instance Prelude.AWSRequest StartStreamEncryption where
  type
    Rs StartStreamEncryption =
      StartStreamEncryptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StartStreamEncryptionResponse'

instance Prelude.Hashable StartStreamEncryption

instance Prelude.NFData StartStreamEncryption

instance Prelude.ToHeaders StartStreamEncryption where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Kinesis_20131202.StartStreamEncryption" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartStreamEncryption where
  toJSON StartStreamEncryption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("StreamName" Prelude..= streamName),
            Prelude.Just
              ("EncryptionType" Prelude..= encryptionType),
            Prelude.Just ("KeyId" Prelude..= keyId)
          ]
      )

instance Prelude.ToPath StartStreamEncryption where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartStreamEncryption where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartStreamEncryptionResponse' smart constructor.
data StartStreamEncryptionResponse = StartStreamEncryptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartStreamEncryptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartStreamEncryptionResponse ::
  StartStreamEncryptionResponse
newStartStreamEncryptionResponse =
  StartStreamEncryptionResponse'

instance Prelude.NFData StartStreamEncryptionResponse
