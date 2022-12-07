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
-- Module      : Amazonka.StorageGateway.CreateTapeWithBarcode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual tape by using your own barcode. You write data to the
-- virtual tape and then archive the tape. A barcode is unique and cannot
-- be reused if it has already been used on a tape. This applies to
-- barcodes used on deleted tapes. This operation is only supported in the
-- tape gateway type.
--
-- Cache storage must be allocated to the gateway before you can create a
-- virtual tape. Use the AddCache operation to add cache storage to a
-- gateway.
module Amazonka.StorageGateway.CreateTapeWithBarcode
  ( -- * Creating a Request
    CreateTapeWithBarcode (..),
    newCreateTapeWithBarcode,

    -- * Request Lenses
    createTapeWithBarcode_tags,
    createTapeWithBarcode_worm,
    createTapeWithBarcode_kmsKey,
    createTapeWithBarcode_kmsEncrypted,
    createTapeWithBarcode_poolId,
    createTapeWithBarcode_gatewayARN,
    createTapeWithBarcode_tapeSizeInBytes,
    createTapeWithBarcode_tapeBarcode,

    -- * Destructuring the Response
    CreateTapeWithBarcodeResponse (..),
    newCreateTapeWithBarcodeResponse,

    -- * Response Lenses
    createTapeWithBarcodeResponse_tapeARN,
    createTapeWithBarcodeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | CreateTapeWithBarcodeInput
--
-- /See:/ 'newCreateTapeWithBarcode' smart constructor.
data CreateTapeWithBarcode = CreateTapeWithBarcode'
  { -- | A list of up to 50 tags that can be assigned to a virtual tape that has
    -- a barcode. Each tag is a key-value pair.
    --
    -- Valid characters for key and value are letters, spaces, and numbers
    -- representable in UTF-8 format, and the following special characters: + -
    -- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
    -- the maximum length for a tag\'s value is 256.
    tags :: Prelude.Maybe [Tag],
    -- | Set to @TRUE@ if the tape you are creating is to be configured as a
    -- write-once-read-many (WORM) tape.
    worm :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
    -- used for Amazon S3 server-side encryption. Storage Gateway does not
    -- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
    -- is @true@. Optional.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
    -- key, or @false@ to use a key managed by Amazon S3. Optional.
    --
    -- Valid Values: @true@ | @false@
    kmsEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the pool that you want to add your tape to for archiving. The
    -- tape in this pool is archived in the S3 storage class that is associated
    -- with the pool. When you use your backup application to eject the tape,
    -- the tape is archived directly into the storage class (S3 Glacier or S3
    -- Deep Archive) that corresponds to the pool.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The unique Amazon Resource Name (ARN) that represents the gateway to
    -- associate the virtual tape with. Use the ListGateways operation to
    -- return a list of gateways for your account and Amazon Web Services
    -- Region.
    gatewayARN :: Prelude.Text,
    -- | The size, in bytes, of the virtual tape that you want to create.
    --
    -- The size must be aligned by gigabyte (1024*1024*1024 bytes).
    tapeSizeInBytes :: Prelude.Integer,
    -- | The barcode that you want to assign to the tape.
    --
    -- Barcodes cannot be reused. This includes barcodes used for tapes that
    -- have been deleted.
    tapeBarcode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTapeWithBarcode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTapeWithBarcode_tags' - A list of up to 50 tags that can be assigned to a virtual tape that has
-- a barcode. Each tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
--
-- 'worm', 'createTapeWithBarcode_worm' - Set to @TRUE@ if the tape you are creating is to be configured as a
-- write-once-read-many (WORM) tape.
--
-- 'kmsKey', 'createTapeWithBarcode_kmsKey' - The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
--
-- 'kmsEncrypted', 'createTapeWithBarcode_kmsEncrypted' - Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
--
-- 'poolId', 'createTapeWithBarcode_poolId' - The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Deep Archive) that corresponds to the pool.
--
-- 'gatewayARN', 'createTapeWithBarcode_gatewayARN' - The unique Amazon Resource Name (ARN) that represents the gateway to
-- associate the virtual tape with. Use the ListGateways operation to
-- return a list of gateways for your account and Amazon Web Services
-- Region.
--
-- 'tapeSizeInBytes', 'createTapeWithBarcode_tapeSizeInBytes' - The size, in bytes, of the virtual tape that you want to create.
--
-- The size must be aligned by gigabyte (1024*1024*1024 bytes).
--
-- 'tapeBarcode', 'createTapeWithBarcode_tapeBarcode' - The barcode that you want to assign to the tape.
--
-- Barcodes cannot be reused. This includes barcodes used for tapes that
-- have been deleted.
newCreateTapeWithBarcode ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'tapeSizeInBytes'
  Prelude.Integer ->
  -- | 'tapeBarcode'
  Prelude.Text ->
  CreateTapeWithBarcode
newCreateTapeWithBarcode
  pGatewayARN_
  pTapeSizeInBytes_
  pTapeBarcode_ =
    CreateTapeWithBarcode'
      { tags = Prelude.Nothing,
        worm = Prelude.Nothing,
        kmsKey = Prelude.Nothing,
        kmsEncrypted = Prelude.Nothing,
        poolId = Prelude.Nothing,
        gatewayARN = pGatewayARN_,
        tapeSizeInBytes = pTapeSizeInBytes_,
        tapeBarcode = pTapeBarcode_
      }

-- | A list of up to 50 tags that can be assigned to a virtual tape that has
-- a barcode. Each tag is a key-value pair.
--
-- Valid characters for key and value are letters, spaces, and numbers
-- representable in UTF-8 format, and the following special characters: + -
-- = . _ : \/ \@. The maximum length of a tag\'s key is 128 characters, and
-- the maximum length for a tag\'s value is 256.
createTapeWithBarcode_tags :: Lens.Lens' CreateTapeWithBarcode (Prelude.Maybe [Tag])
createTapeWithBarcode_tags = Lens.lens (\CreateTapeWithBarcode' {tags} -> tags) (\s@CreateTapeWithBarcode' {} a -> s {tags = a} :: CreateTapeWithBarcode) Prelude.. Lens.mapping Lens.coerced

-- | Set to @TRUE@ if the tape you are creating is to be configured as a
-- write-once-read-many (WORM) tape.
createTapeWithBarcode_worm :: Lens.Lens' CreateTapeWithBarcode (Prelude.Maybe Prelude.Bool)
createTapeWithBarcode_worm = Lens.lens (\CreateTapeWithBarcode' {worm} -> worm) (\s@CreateTapeWithBarcode' {} a -> s {worm = a} :: CreateTapeWithBarcode)

-- | The Amazon Resource Name (ARN) of a symmetric customer master key (CMK)
-- used for Amazon S3 server-side encryption. Storage Gateway does not
-- support asymmetric CMKs. This value can only be set when @KMSEncrypted@
-- is @true@. Optional.
createTapeWithBarcode_kmsKey :: Lens.Lens' CreateTapeWithBarcode (Prelude.Maybe Prelude.Text)
createTapeWithBarcode_kmsKey = Lens.lens (\CreateTapeWithBarcode' {kmsKey} -> kmsKey) (\s@CreateTapeWithBarcode' {} a -> s {kmsKey = a} :: CreateTapeWithBarcode)

-- | Set to @true@ to use Amazon S3 server-side encryption with your own KMS
-- key, or @false@ to use a key managed by Amazon S3. Optional.
--
-- Valid Values: @true@ | @false@
createTapeWithBarcode_kmsEncrypted :: Lens.Lens' CreateTapeWithBarcode (Prelude.Maybe Prelude.Bool)
createTapeWithBarcode_kmsEncrypted = Lens.lens (\CreateTapeWithBarcode' {kmsEncrypted} -> kmsEncrypted) (\s@CreateTapeWithBarcode' {} a -> s {kmsEncrypted = a} :: CreateTapeWithBarcode)

-- | The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Deep Archive) that corresponds to the pool.
createTapeWithBarcode_poolId :: Lens.Lens' CreateTapeWithBarcode (Prelude.Maybe Prelude.Text)
createTapeWithBarcode_poolId = Lens.lens (\CreateTapeWithBarcode' {poolId} -> poolId) (\s@CreateTapeWithBarcode' {} a -> s {poolId = a} :: CreateTapeWithBarcode)

-- | The unique Amazon Resource Name (ARN) that represents the gateway to
-- associate the virtual tape with. Use the ListGateways operation to
-- return a list of gateways for your account and Amazon Web Services
-- Region.
createTapeWithBarcode_gatewayARN :: Lens.Lens' CreateTapeWithBarcode Prelude.Text
createTapeWithBarcode_gatewayARN = Lens.lens (\CreateTapeWithBarcode' {gatewayARN} -> gatewayARN) (\s@CreateTapeWithBarcode' {} a -> s {gatewayARN = a} :: CreateTapeWithBarcode)

-- | The size, in bytes, of the virtual tape that you want to create.
--
-- The size must be aligned by gigabyte (1024*1024*1024 bytes).
createTapeWithBarcode_tapeSizeInBytes :: Lens.Lens' CreateTapeWithBarcode Prelude.Integer
createTapeWithBarcode_tapeSizeInBytes = Lens.lens (\CreateTapeWithBarcode' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@CreateTapeWithBarcode' {} a -> s {tapeSizeInBytes = a} :: CreateTapeWithBarcode)

-- | The barcode that you want to assign to the tape.
--
-- Barcodes cannot be reused. This includes barcodes used for tapes that
-- have been deleted.
createTapeWithBarcode_tapeBarcode :: Lens.Lens' CreateTapeWithBarcode Prelude.Text
createTapeWithBarcode_tapeBarcode = Lens.lens (\CreateTapeWithBarcode' {tapeBarcode} -> tapeBarcode) (\s@CreateTapeWithBarcode' {} a -> s {tapeBarcode = a} :: CreateTapeWithBarcode)

instance Core.AWSRequest CreateTapeWithBarcode where
  type
    AWSResponse CreateTapeWithBarcode =
      CreateTapeWithBarcodeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTapeWithBarcodeResponse'
            Prelude.<$> (x Data..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTapeWithBarcode where
  hashWithSalt _salt CreateTapeWithBarcode' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` worm
      `Prelude.hashWithSalt` kmsKey
      `Prelude.hashWithSalt` kmsEncrypted
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` tapeSizeInBytes
      `Prelude.hashWithSalt` tapeBarcode

instance Prelude.NFData CreateTapeWithBarcode where
  rnf CreateTapeWithBarcode' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf worm
      `Prelude.seq` Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf kmsEncrypted
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf tapeSizeInBytes
      `Prelude.seq` Prelude.rnf tapeBarcode

instance Data.ToHeaders CreateTapeWithBarcode where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.CreateTapeWithBarcode" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTapeWithBarcode where
  toJSON CreateTapeWithBarcode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("Worm" Data..=) Prelude.<$> worm,
            ("KMSKey" Data..=) Prelude.<$> kmsKey,
            ("KMSEncrypted" Data..=) Prelude.<$> kmsEncrypted,
            ("PoolId" Data..=) Prelude.<$> poolId,
            Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just
              ("TapeSizeInBytes" Data..= tapeSizeInBytes),
            Prelude.Just ("TapeBarcode" Data..= tapeBarcode)
          ]
      )

instance Data.ToPath CreateTapeWithBarcode where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTapeWithBarcode where
  toQuery = Prelude.const Prelude.mempty

-- | CreateTapeOutput
--
-- /See:/ 'newCreateTapeWithBarcodeResponse' smart constructor.
data CreateTapeWithBarcodeResponse = CreateTapeWithBarcodeResponse'
  { -- | A unique Amazon Resource Name (ARN) that represents the virtual tape
    -- that was created.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTapeWithBarcodeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'createTapeWithBarcodeResponse_tapeARN' - A unique Amazon Resource Name (ARN) that represents the virtual tape
-- that was created.
--
-- 'httpStatus', 'createTapeWithBarcodeResponse_httpStatus' - The response's http status code.
newCreateTapeWithBarcodeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTapeWithBarcodeResponse
newCreateTapeWithBarcodeResponse pHttpStatus_ =
  CreateTapeWithBarcodeResponse'
    { tapeARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique Amazon Resource Name (ARN) that represents the virtual tape
-- that was created.
createTapeWithBarcodeResponse_tapeARN :: Lens.Lens' CreateTapeWithBarcodeResponse (Prelude.Maybe Prelude.Text)
createTapeWithBarcodeResponse_tapeARN = Lens.lens (\CreateTapeWithBarcodeResponse' {tapeARN} -> tapeARN) (\s@CreateTapeWithBarcodeResponse' {} a -> s {tapeARN = a} :: CreateTapeWithBarcodeResponse)

-- | The response's http status code.
createTapeWithBarcodeResponse_httpStatus :: Lens.Lens' CreateTapeWithBarcodeResponse Prelude.Int
createTapeWithBarcodeResponse_httpStatus = Lens.lens (\CreateTapeWithBarcodeResponse' {httpStatus} -> httpStatus) (\s@CreateTapeWithBarcodeResponse' {} a -> s {httpStatus = a} :: CreateTapeWithBarcodeResponse)

instance Prelude.NFData CreateTapeWithBarcodeResponse where
  rnf CreateTapeWithBarcodeResponse' {..} =
    Prelude.rnf tapeARN
      `Prelude.seq` Prelude.rnf httpStatus
