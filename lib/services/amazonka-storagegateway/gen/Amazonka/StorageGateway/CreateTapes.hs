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
-- Module      : Amazonka.StorageGateway.CreateTapes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more virtual tapes. You write data to the virtual tapes
-- and then archive the tapes. This operation is only supported in the tape
-- gateway type.
--
-- Cache storage must be allocated to the gateway before you can create
-- virtual tapes. Use the AddCache operation to add cache storage to a
-- gateway.
module Amazonka.StorageGateway.CreateTapes
  ( -- * Creating a Request
    CreateTapes (..),
    newCreateTapes,

    -- * Request Lenses
    createTapes_kmsEncrypted,
    createTapes_kmsKey,
    createTapes_poolId,
    createTapes_tags,
    createTapes_worm,
    createTapes_gatewayARN,
    createTapes_tapeSizeInBytes,
    createTapes_clientToken,
    createTapes_numTapesToCreate,
    createTapes_tapeBarcodePrefix,

    -- * Destructuring the Response
    CreateTapesResponse (..),
    newCreateTapesResponse,

    -- * Response Lenses
    createTapesResponse_tapeARNs,
    createTapesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | CreateTapesInput
--
-- /See:/ 'newCreateTapes' smart constructor.
data CreateTapes = CreateTapes'
  { -- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
    -- key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The ID of the pool that you want to add your tape to for archiving. The
    -- tape in this pool is archived in the S3 storage class that is associated
    -- with the pool. When you use your backup application to eject the tape,
    -- the tape is archived directly into the storage class (S3 Glacier or S3
    -- Glacier Deep Archive) that corresponds to the pool.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 50 tags that can be assigned to a virtual tape. Each tag
    -- is a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
    -- | Set to @TRUE@ if the tape you are creating is to be configured as a
    -- write-once-read-many (WORM) tape.
    worm :: Prelude.Maybe Prelude.Bool,
    -- | The unique Amazon Resource Name (ARN) that represents the gateway to
    -- associate the virtual tapes with. Use the ListGateways operation to
    -- return a list of gateways for your account and Amazon Web Services
    -- Region.
    gatewayARN :: Prelude.Text,
    -- | The size, in bytes, of the virtual tapes that you want to create.
    --
    -- The size must be aligned by gigabyte (1024*1024*1024 bytes).
    tapeSizeInBytes :: Prelude.Integer,
    -- | A unique identifier that you use to retry a request. If you retry a
    -- request, use the same @ClientToken@ you specified in the initial
    -- request.
    --
    -- Using the same @ClientToken@ prevents creating the tape multiple times.
    clientToken :: Prelude.Text,
    -- | The number of virtual tapes that you want to create.
    numTapesToCreate :: Prelude.Natural,
    -- | A prefix that you append to the barcode of the virtual tape you are
    -- creating. This prefix makes the barcode unique.
    --
    -- The prefix must be 1-4 characters in length and must be one of the
    -- uppercase letters from A to Z.
    tapeBarcodePrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTapes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsEncrypted', 'createTapes_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsKey', 'createTapes_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'poolId', 'createTapes_poolId' - The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- 'tags', 'createTapes_tags' - A list of up to 50 tags that can be assigned to a virtual tape. Each tag
-- is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'worm', 'createTapes_worm' - Set to @TRUE@ if the tape you are creating is to be configured as a
-- write-once-read-many (WORM) tape.
--
-- 'gatewayARN', 'createTapes_gatewayARN' - The unique Amazon Resource Name (ARN) that represents the gateway to
-- associate the virtual tapes with. Use the ListGateways operation to
-- return a list of gateways for your account and Amazon Web Services
-- Region.
--
-- 'tapeSizeInBytes', 'createTapes_tapeSizeInBytes' - The size, in bytes, of the virtual tapes that you want to create.
--
-- The size must be aligned by gigabyte (1024*1024*1024 bytes).
--
-- 'clientToken', 'createTapes_clientToken' - A unique identifier that you use to retry a request. If you retry a
-- request, use the same @ClientToken@ you specified in the initial
-- request.
--
-- Using the same @ClientToken@ prevents creating the tape multiple times.
--
-- 'numTapesToCreate', 'createTapes_numTapesToCreate' - The number of virtual tapes that you want to create.
--
-- 'tapeBarcodePrefix', 'createTapes_tapeBarcodePrefix' - A prefix that you append to the barcode of the virtual tape you are
-- creating. This prefix makes the barcode unique.
--
-- The prefix must be 1-4 characters in length and must be one of the
-- uppercase letters from A to Z.
newCreateTapes ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'tapeSizeInBytes'
  Prelude.Integer ->
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'numTapesToCreate'
  Prelude.Natural ->
  -- | 'tapeBarcodePrefix'
  Prelude.Text ->
  CreateTapes
newCreateTapes
  pGatewayARN_
  pTapeSizeInBytes_
  pClientToken_
  pNumTapesToCreate_
  pTapeBarcodePrefix_ =
    CreateTapes'
      { kmsEncrypted = Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        poolId = Prelude.Nothing,
        tags = Prelude.Nothing,
        worm = Prelude.Nothing,
        gatewayARN = pGatewayARN_,
        tapeSizeInBytes = pTapeSizeInBytes_,
        clientToken = pClientToken_,
        numTapesToCreate = pNumTapesToCreate_,
        tapeBarcodePrefix = pTapeBarcodePrefix_
      }

-- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createTapes_kmsEncrypted :: Lens.Lens' CreateTapes (Prelude.Maybe Prelude.Bool)
createTapes_kmsEncrypted = Lens.lens (\CreateTapes' {kmsEncrypted} -> kmsEncrypted) (\s@CreateTapes' {} a -> s {kmsEncrypted = a} :: CreateTapes)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createTapes_kmsKey :: Lens.Lens' CreateTapes (Prelude.Maybe Prelude.Text)
createTapes_kmsKey = Lens.lens (\CreateTapes' {kmsKey} -> kmsKey) (\s@CreateTapes' {} a -> s {kmsKey = a} :: CreateTapes)

-- | The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
createTapes_poolId :: Lens.Lens' CreateTapes (Prelude.Maybe Prelude.Text)
createTapes_poolId = Lens.lens (\CreateTapes' {poolId} -> poolId) (\s@CreateTapes' {} a -> s {poolId = a} :: CreateTapes)

-- | A list of up to 50 tags that can be assigned to a virtual tape. Each tag
-- is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createTapes_tags :: Lens.Lens' CreateTapes (Prelude.Maybe [Tag])
createTapes_tags = Lens.lens (\CreateTapes' {tags} -> tags) (\s@CreateTapes' {} a -> s {tags = a} :: CreateTapes) Prelude.. Lens.mapping Lens.coerced

-- | Set to @TRUE@ if the tape you are creating is to be configured as a
-- write-once-read-many (WORM) tape.
createTapes_worm :: Lens.Lens' CreateTapes (Prelude.Maybe Prelude.Bool)
createTapes_worm = Lens.lens (\CreateTapes' {worm} -> worm) (\s@CreateTapes' {} a -> s {worm = a} :: CreateTapes)

-- | The unique Amazon Resource Name (ARN) that represents the gateway to
-- associate the virtual tapes with. Use the ListGateways operation to
-- return a list of gateways for your account and Amazon Web Services
-- Region.
createTapes_gatewayARN :: Lens.Lens' CreateTapes Prelude.Text
createTapes_gatewayARN = Lens.lens (\CreateTapes' {gatewayARN} -> gatewayARN) (\s@CreateTapes' {} a -> s {gatewayARN = a} :: CreateTapes)

-- | The size, in bytes, of the virtual tapes that you want to create.
--
-- The size must be aligned by gigabyte (1024*1024*1024 bytes).
createTapes_tapeSizeInBytes :: Lens.Lens' CreateTapes Prelude.Integer
createTapes_tapeSizeInBytes = Lens.lens (\CreateTapes' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@CreateTapes' {} a -> s {tapeSizeInBytes = a} :: CreateTapes)

-- | A unique identifier that you use to retry a request. If you retry a
-- request, use the same @ClientToken@ you specified in the initial
-- request.
--
-- Using the same @ClientToken@ prevents creating the tape multiple times.
createTapes_clientToken :: Lens.Lens' CreateTapes Prelude.Text
createTapes_clientToken = Lens.lens (\CreateTapes' {clientToken} -> clientToken) (\s@CreateTapes' {} a -> s {clientToken = a} :: CreateTapes)

-- | The number of virtual tapes that you want to create.
createTapes_numTapesToCreate :: Lens.Lens' CreateTapes Prelude.Natural
createTapes_numTapesToCreate = Lens.lens (\CreateTapes' {numTapesToCreate} -> numTapesToCreate) (\s@CreateTapes' {} a -> s {numTapesToCreate = a} :: CreateTapes)

-- | A prefix that you append to the barcode of the virtual tape you are
-- creating. This prefix makes the barcode unique.
--
-- The prefix must be 1-4 characters in length and must be one of the
-- uppercase letters from A to Z.
createTapes_tapeBarcodePrefix :: Lens.Lens' CreateTapes Prelude.Text
createTapes_tapeBarcodePrefix = Lens.lens (\CreateTapes' {tapeBarcodePrefix} -> tapeBarcodePrefix) (\s@CreateTapes' {} a -> s {tapeBarcodePrefix = a} :: CreateTapes)

instance Core.AWSRequest CreateTapes where
  type AWSResponse CreateTapes = CreateTapesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTapesResponse'
            Prelude.<$> (x Data..?> "TapeARNs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTapes where
  hashWithSalt _salt CreateTapes' {..} =
    _salt
      `Prelude.hashWithSalt` kmsEncrypted
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` worm
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` tapeSizeInBytes
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` numTapesToCreate
      `Prelude.hashWithSalt` tapeBarcodePrefix

instance Prelude.NFData CreateTapes where
  rnf CreateTapes' {..} =
    Prelude.rnf kmsEncrypted
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf worm
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf tapeSizeInBytes
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf numTapesToCreate
      `Prelude.seq` Prelude.rnf tapeBarcodePrefix

instance Data.ToHeaders CreateTapes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.CreateTapes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTapes where
  toJSON CreateTapes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KMSEncrypted" Data..=) Prelude.<$> kmsEncrypted,
            ("KMSKey" Data..=) Prelude.<$> kmsKey,
            ("PoolId" Data..=) Prelude.<$> poolId,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Worm" Data..=) Prelude.<$> worm,
            Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just
              ("TapeSizeInBytes" Data..= tapeSizeInBytes),
            Prelude.Just ("ClientToken" Data..= clientToken),
            Prelude.Just
              ("NumTapesToCreate" Data..= numTapesToCreate),
            Prelude.Just
              ("TapeBarcodePrefix" Data..= tapeBarcodePrefix)
          ]
      )

instance Data.ToPath CreateTapes where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTapes where
  toQuery = Prelude.const Prelude.mempty

-- | CreateTapeOutput
--
-- /See:/ 'newCreateTapesResponse' smart constructor.
data CreateTapesResponse = CreateTapesResponse'
  { -- | A list of unique Amazon Resource Names (ARNs) that represents the
    -- virtual tapes that were created.
    tapeARNs :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTapesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARNs', 'createTapesResponse_tapeARNs' - A list of unique Amazon Resource Names (ARNs) that represents the
-- virtual tapes that were created.
--
-- 'httpStatus', 'createTapesResponse_httpStatus' - The response's http status code.
newCreateTapesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTapesResponse
newCreateTapesResponse pHttpStatus_ =
  CreateTapesResponse'
    { tapeARNs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of unique Amazon Resource Names (ARNs) that represents the
-- virtual tapes that were created.
createTapesResponse_tapeARNs :: Lens.Lens' CreateTapesResponse (Prelude.Maybe [Prelude.Text])
createTapesResponse_tapeARNs = Lens.lens (\CreateTapesResponse' {tapeARNs} -> tapeARNs) (\s@CreateTapesResponse' {} a -> s {tapeARNs = a} :: CreateTapesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createTapesResponse_httpStatus :: Lens.Lens' CreateTapesResponse Prelude.Int
createTapesResponse_httpStatus = Lens.lens (\CreateTapesResponse' {httpStatus} -> httpStatus) (\s@CreateTapesResponse' {} a -> s {httpStatus = a} :: CreateTapesResponse)

instance Prelude.NFData CreateTapesResponse where
  rnf CreateTapesResponse' {..} =
    Prelude.rnf tapeARNs
      `Prelude.seq` Prelude.rnf httpStatus
