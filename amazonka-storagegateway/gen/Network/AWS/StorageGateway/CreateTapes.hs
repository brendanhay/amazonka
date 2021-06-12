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
-- Module      : Network.AWS.StorageGateway.CreateTapes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.StorageGateway.CreateTapes
  ( -- * Creating a Request
    CreateTapes (..),
    newCreateTapes,

    -- * Request Lenses
    createTapes_poolId,
    createTapes_kmsEncrypted,
    createTapes_kmsKey,
    createTapes_worm,
    createTapes_tags,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | CreateTapesInput
--
-- /See:/ 'newCreateTapes' smart constructor.
data CreateTapes = CreateTapes'
  { -- | The ID of the pool that you want to add your tape to for archiving. The
    -- tape in this pool is archived in the S3 storage class that is associated
    -- with the pool. When you use your backup application to eject the tape,
    -- the tape is archived directly into the storage class (S3 Glacier or S3
    -- Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Core.Maybe Core.Text,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
    -- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Core.Maybe Core.Text,
    -- | Set to @TRUE@ if the tape you are creating is to be configured as a
    -- write-once-read-many (WORM) tape.
    worm :: Core.Maybe Core.Bool,
    -- | A list of up to 50 tags that can be assigned to a virtual tape. Each tag
    -- is a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Core.Maybe [Tag],
    -- | The unique Amazon Resource Name (ARN) that represents the gateway to
    -- associate the virtual tapes with. Use the ListGateways operation to
    -- return a list of gateways for your account and AWS Region.
    gatewayARN :: Core.Text,
    -- | The size, in bytes, of the virtual tapes that you want to create.
    --
    -- The size must be aligned by gigabyte (1024*1024*1024 bytes).
    tapeSizeInBytes :: Core.Integer,
    -- | A unique identifier that you use to retry a request. If you retry a
    -- request, use the same @ClientToken@ you specified in the initial
    -- request.
    --
    -- Using the same @ClientToken@ prevents creating the tape multiple times.
    clientToken :: Core.Text,
    -- | The number of virtual tapes that you want to create.
    numTapesToCreate :: Core.Natural,
    -- | A prefix that you append to the barcode of the virtual tape you are
    -- creating. This prefix makes the barcode unique.
    --
    -- The prefix must be 1 to 4 characters in length and must be one of the
    -- uppercase letters from A to Z.
    tapeBarcodePrefix :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTapes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolId', 'createTapes_poolId' - The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- 'kmsEncrypted', 'createTapes_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'kmsKey', 'createTapes_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'worm', 'createTapes_worm' - Set to @TRUE@ if the tape you are creating is to be configured as a
-- write-once-read-many (WORM) tape.
--
-- 'tags', 'createTapes_tags' - A list of up to 50 tags that can be assigned to a virtual tape. Each tag
-- is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'gatewayARN', 'createTapes_gatewayARN' - The unique Amazon Resource Name (ARN) that represents the gateway to
-- associate the virtual tapes with. Use the ListGateways operation to
-- return a list of gateways for your account and AWS Region.
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
-- The prefix must be 1 to 4 characters in length and must be one of the
-- uppercase letters from A to Z.
newCreateTapes ::
  -- | 'gatewayARN'
  Core.Text ->
  -- | 'tapeSizeInBytes'
  Core.Integer ->
  -- | 'clientToken'
  Core.Text ->
  -- | 'numTapesToCreate'
  Core.Natural ->
  -- | 'tapeBarcodePrefix'
  Core.Text ->
  CreateTapes
newCreateTapes
  pGatewayARN_
  pTapeSizeInBytes_
  pClientToken_
  pNumTapesToCreate_
  pTapeBarcodePrefix_ =
    CreateTapes'
      { poolId = Core.Nothing,
        kmsEncrypted = Core.Nothing,
        kmsKey = Core.Nothing,
        worm = Core.Nothing,
        tags = Core.Nothing,
        gatewayARN = pGatewayARN_,
        tapeSizeInBytes = pTapeSizeInBytes_,
        clientToken = pClientToken_,
        numTapesToCreate = pNumTapesToCreate_,
        tapeBarcodePrefix = pTapeBarcodePrefix_
      }

-- | The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
createTapes_poolId :: Lens.Lens' CreateTapes (Core.Maybe Core.Text)
createTapes_poolId = Lens.lens (\CreateTapes' {poolId} -> poolId) (\s@CreateTapes' {} a -> s {poolId = a} :: CreateTapes)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own AWS
-- KMS key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createTapes_kmsEncrypted :: Lens.Lens' CreateTapes (Core.Maybe Core.Bool)
createTapes_kmsEncrypted = Lens.lens (\CreateTapes' {kmsEncrypted} -> kmsEncrypted) (\s@CreateTapes' {} a -> s {kmsEncrypted = a} :: CreateTapes)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createTapes_kmsKey :: Lens.Lens' CreateTapes (Core.Maybe Core.Text)
createTapes_kmsKey = Lens.lens (\CreateTapes' {kmsKey} -> kmsKey) (\s@CreateTapes' {} a -> s {kmsKey = a} :: CreateTapes)

-- | Set to @TRUE@ if the tape you are creating is to be configured as a
-- write-once-read-many (WORM) tape.
createTapes_worm :: Lens.Lens' CreateTapes (Core.Maybe Core.Bool)
createTapes_worm = Lens.lens (\CreateTapes' {worm} -> worm) (\s@CreateTapes' {} a -> s {worm = a} :: CreateTapes)

-- | A list of up to 50 tags that can be assigned to a virtual tape. Each tag
-- is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createTapes_tags :: Lens.Lens' CreateTapes (Core.Maybe [Tag])
createTapes_tags = Lens.lens (\CreateTapes' {tags} -> tags) (\s@CreateTapes' {} a -> s {tags = a} :: CreateTapes) Core.. Lens.mapping Lens._Coerce

-- | The unique Amazon Resource Name (ARN) that represents the gateway to
-- associate the virtual tapes with. Use the ListGateways operation to
-- return a list of gateways for your account and AWS Region.
createTapes_gatewayARN :: Lens.Lens' CreateTapes Core.Text
createTapes_gatewayARN = Lens.lens (\CreateTapes' {gatewayARN} -> gatewayARN) (\s@CreateTapes' {} a -> s {gatewayARN = a} :: CreateTapes)

-- | The size, in bytes, of the virtual tapes that you want to create.
--
-- The size must be aligned by gigabyte (1024*1024*1024 bytes).
createTapes_tapeSizeInBytes :: Lens.Lens' CreateTapes Core.Integer
createTapes_tapeSizeInBytes = Lens.lens (\CreateTapes' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@CreateTapes' {} a -> s {tapeSizeInBytes = a} :: CreateTapes)

-- | A unique identifier that you use to retry a request. If you retry a
-- request, use the same @ClientToken@ you specified in the initial
-- request.
--
-- Using the same @ClientToken@ prevents creating the tape multiple times.
createTapes_clientToken :: Lens.Lens' CreateTapes Core.Text
createTapes_clientToken = Lens.lens (\CreateTapes' {clientToken} -> clientToken) (\s@CreateTapes' {} a -> s {clientToken = a} :: CreateTapes)

-- | The number of virtual tapes that you want to create.
createTapes_numTapesToCreate :: Lens.Lens' CreateTapes Core.Natural
createTapes_numTapesToCreate = Lens.lens (\CreateTapes' {numTapesToCreate} -> numTapesToCreate) (\s@CreateTapes' {} a -> s {numTapesToCreate = a} :: CreateTapes)

-- | A prefix that you append to the barcode of the virtual tape you are
-- creating. This prefix makes the barcode unique.
--
-- The prefix must be 1 to 4 characters in length and must be one of the
-- uppercase letters from A to Z.
createTapes_tapeBarcodePrefix :: Lens.Lens' CreateTapes Core.Text
createTapes_tapeBarcodePrefix = Lens.lens (\CreateTapes' {tapeBarcodePrefix} -> tapeBarcodePrefix) (\s@CreateTapes' {} a -> s {tapeBarcodePrefix = a} :: CreateTapes)

instance Core.AWSRequest CreateTapes where
  type AWSResponse CreateTapes = CreateTapesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTapesResponse'
            Core.<$> (x Core..?> "TapeARNs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTapes

instance Core.NFData CreateTapes

instance Core.ToHeaders CreateTapes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.CreateTapes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTapes where
  toJSON CreateTapes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PoolId" Core..=) Core.<$> poolId,
            ("KMSEncrypted" Core..=) Core.<$> kmsEncrypted,
            ("KMSKey" Core..=) Core.<$> kmsKey,
            ("Worm" Core..=) Core.<$> worm,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just
              ("TapeSizeInBytes" Core..= tapeSizeInBytes),
            Core.Just ("ClientToken" Core..= clientToken),
            Core.Just
              ("NumTapesToCreate" Core..= numTapesToCreate),
            Core.Just
              ("TapeBarcodePrefix" Core..= tapeBarcodePrefix)
          ]
      )

instance Core.ToPath CreateTapes where
  toPath = Core.const "/"

instance Core.ToQuery CreateTapes where
  toQuery = Core.const Core.mempty

-- | CreateTapeOutput
--
-- /See:/ 'newCreateTapesResponse' smart constructor.
data CreateTapesResponse = CreateTapesResponse'
  { -- | A list of unique Amazon Resource Names (ARNs) that represents the
    -- virtual tapes that were created.
    tapeARNs :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateTapesResponse
newCreateTapesResponse pHttpStatus_ =
  CreateTapesResponse'
    { tapeARNs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of unique Amazon Resource Names (ARNs) that represents the
-- virtual tapes that were created.
createTapesResponse_tapeARNs :: Lens.Lens' CreateTapesResponse (Core.Maybe [Core.Text])
createTapesResponse_tapeARNs = Lens.lens (\CreateTapesResponse' {tapeARNs} -> tapeARNs) (\s@CreateTapesResponse' {} a -> s {tapeARNs = a} :: CreateTapesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createTapesResponse_httpStatus :: Lens.Lens' CreateTapesResponse Core.Int
createTapesResponse_httpStatus = Lens.lens (\CreateTapesResponse' {httpStatus} -> httpStatus) (\s@CreateTapesResponse' {} a -> s {httpStatus = a} :: CreateTapesResponse)

instance Core.NFData CreateTapesResponse
