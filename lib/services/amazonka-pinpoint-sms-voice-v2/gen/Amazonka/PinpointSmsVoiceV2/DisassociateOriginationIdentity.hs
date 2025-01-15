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
-- Module      : Amazonka.PinpointSmsVoiceV2.DisassociateOriginationIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified origination identity from an existing pool.
--
-- If the origination identity isn\'t associated with the specified pool,
-- an Error is returned.
module Amazonka.PinpointSmsVoiceV2.DisassociateOriginationIdentity
  ( -- * Creating a Request
    DisassociateOriginationIdentity (..),
    newDisassociateOriginationIdentity,

    -- * Request Lenses
    disassociateOriginationIdentity_clientToken,
    disassociateOriginationIdentity_poolId,
    disassociateOriginationIdentity_originationIdentity,
    disassociateOriginationIdentity_isoCountryCode,

    -- * Destructuring the Response
    DisassociateOriginationIdentityResponse (..),
    newDisassociateOriginationIdentityResponse,

    -- * Response Lenses
    disassociateOriginationIdentityResponse_isoCountryCode,
    disassociateOriginationIdentityResponse_originationIdentity,
    disassociateOriginationIdentityResponse_originationIdentityArn,
    disassociateOriginationIdentityResponse_poolArn,
    disassociateOriginationIdentityResponse_poolId,
    disassociateOriginationIdentityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateOriginationIdentity' smart constructor.
data DisassociateOriginationIdentity = DisassociateOriginationIdentity'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. If you don\'t specify a client token, a randomly
    -- generated token is used for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the pool to disassociate with the origination
    -- identity. This value can be either the PoolId or PoolArn.
    poolId :: Prelude.Text,
    -- | The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
    -- SenderId or SenderIdArn. You can use DescribePhoneNumbers find the
    -- values for PhoneNumberId and PhoneNumberArn, or use DescribeSenderIds to
    -- get the values for SenderId and SenderIdArn.
    originationIdentity :: Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateOriginationIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disassociateOriginationIdentity_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you don\'t specify a client token, a randomly
-- generated token is used for the request to ensure idempotency.
--
-- 'poolId', 'disassociateOriginationIdentity_poolId' - The unique identifier for the pool to disassociate with the origination
-- identity. This value can be either the PoolId or PoolArn.
--
-- 'originationIdentity', 'disassociateOriginationIdentity_originationIdentity' - The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
-- SenderId or SenderIdArn. You can use DescribePhoneNumbers find the
-- values for PhoneNumberId and PhoneNumberArn, or use DescribeSenderIds to
-- get the values for SenderId and SenderIdArn.
--
-- 'isoCountryCode', 'disassociateOriginationIdentity_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
newDisassociateOriginationIdentity ::
  -- | 'poolId'
  Prelude.Text ->
  -- | 'originationIdentity'
  Prelude.Text ->
  -- | 'isoCountryCode'
  Prelude.Text ->
  DisassociateOriginationIdentity
newDisassociateOriginationIdentity
  pPoolId_
  pOriginationIdentity_
  pIsoCountryCode_ =
    DisassociateOriginationIdentity'
      { clientToken =
          Prelude.Nothing,
        poolId = pPoolId_,
        originationIdentity =
          pOriginationIdentity_,
        isoCountryCode = pIsoCountryCode_
      }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. If you don\'t specify a client token, a randomly
-- generated token is used for the request to ensure idempotency.
disassociateOriginationIdentity_clientToken :: Lens.Lens' DisassociateOriginationIdentity (Prelude.Maybe Prelude.Text)
disassociateOriginationIdentity_clientToken = Lens.lens (\DisassociateOriginationIdentity' {clientToken} -> clientToken) (\s@DisassociateOriginationIdentity' {} a -> s {clientToken = a} :: DisassociateOriginationIdentity)

-- | The unique identifier for the pool to disassociate with the origination
-- identity. This value can be either the PoolId or PoolArn.
disassociateOriginationIdentity_poolId :: Lens.Lens' DisassociateOriginationIdentity Prelude.Text
disassociateOriginationIdentity_poolId = Lens.lens (\DisassociateOriginationIdentity' {poolId} -> poolId) (\s@DisassociateOriginationIdentity' {} a -> s {poolId = a} :: DisassociateOriginationIdentity)

-- | The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
-- SenderId or SenderIdArn. You can use DescribePhoneNumbers find the
-- values for PhoneNumberId and PhoneNumberArn, or use DescribeSenderIds to
-- get the values for SenderId and SenderIdArn.
disassociateOriginationIdentity_originationIdentity :: Lens.Lens' DisassociateOriginationIdentity Prelude.Text
disassociateOriginationIdentity_originationIdentity = Lens.lens (\DisassociateOriginationIdentity' {originationIdentity} -> originationIdentity) (\s@DisassociateOriginationIdentity' {} a -> s {originationIdentity = a} :: DisassociateOriginationIdentity)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
disassociateOriginationIdentity_isoCountryCode :: Lens.Lens' DisassociateOriginationIdentity Prelude.Text
disassociateOriginationIdentity_isoCountryCode = Lens.lens (\DisassociateOriginationIdentity' {isoCountryCode} -> isoCountryCode) (\s@DisassociateOriginationIdentity' {} a -> s {isoCountryCode = a} :: DisassociateOriginationIdentity)

instance
  Core.AWSRequest
    DisassociateOriginationIdentity
  where
  type
    AWSResponse DisassociateOriginationIdentity =
      DisassociateOriginationIdentityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateOriginationIdentityResponse'
            Prelude.<$> (x Data..?> "IsoCountryCode")
            Prelude.<*> (x Data..?> "OriginationIdentity")
            Prelude.<*> (x Data..?> "OriginationIdentityArn")
            Prelude.<*> (x Data..?> "PoolArn")
            Prelude.<*> (x Data..?> "PoolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateOriginationIdentity
  where
  hashWithSalt
    _salt
    DisassociateOriginationIdentity' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` poolId
        `Prelude.hashWithSalt` originationIdentity
        `Prelude.hashWithSalt` isoCountryCode

instance
  Prelude.NFData
    DisassociateOriginationIdentity
  where
  rnf DisassociateOriginationIdentity' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf poolId `Prelude.seq`
        Prelude.rnf originationIdentity `Prelude.seq`
          Prelude.rnf isoCountryCode

instance
  Data.ToHeaders
    DisassociateOriginationIdentity
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DisassociateOriginationIdentity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateOriginationIdentity where
  toJSON DisassociateOriginationIdentity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("PoolId" Data..= poolId),
            Prelude.Just
              ("OriginationIdentity" Data..= originationIdentity),
            Prelude.Just
              ("IsoCountryCode" Data..= isoCountryCode)
          ]
      )

instance Data.ToPath DisassociateOriginationIdentity where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateOriginationIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateOriginationIdentityResponse' smart constructor.
data DisassociateOriginationIdentityResponse = DisassociateOriginationIdentityResponse'
  { -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberId or SenderId of the origination identity.
    originationIdentity :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberArn or SenderIdArn of the origination identity.
    originationIdentityArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pool.
    poolArn :: Prelude.Maybe Prelude.Text,
    -- | The PoolId of the pool no longer associated with the origination
    -- identity.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateOriginationIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isoCountryCode', 'disassociateOriginationIdentityResponse_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
--
-- 'originationIdentity', 'disassociateOriginationIdentityResponse_originationIdentity' - The PhoneNumberId or SenderId of the origination identity.
--
-- 'originationIdentityArn', 'disassociateOriginationIdentityResponse_originationIdentityArn' - The PhoneNumberArn or SenderIdArn of the origination identity.
--
-- 'poolArn', 'disassociateOriginationIdentityResponse_poolArn' - The Amazon Resource Name (ARN) of the pool.
--
-- 'poolId', 'disassociateOriginationIdentityResponse_poolId' - The PoolId of the pool no longer associated with the origination
-- identity.
--
-- 'httpStatus', 'disassociateOriginationIdentityResponse_httpStatus' - The response's http status code.
newDisassociateOriginationIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateOriginationIdentityResponse
newDisassociateOriginationIdentityResponse
  pHttpStatus_ =
    DisassociateOriginationIdentityResponse'
      { isoCountryCode =
          Prelude.Nothing,
        originationIdentity =
          Prelude.Nothing,
        originationIdentityArn =
          Prelude.Nothing,
        poolArn = Prelude.Nothing,
        poolId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
disassociateOriginationIdentityResponse_isoCountryCode :: Lens.Lens' DisassociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
disassociateOriginationIdentityResponse_isoCountryCode = Lens.lens (\DisassociateOriginationIdentityResponse' {isoCountryCode} -> isoCountryCode) (\s@DisassociateOriginationIdentityResponse' {} a -> s {isoCountryCode = a} :: DisassociateOriginationIdentityResponse)

-- | The PhoneNumberId or SenderId of the origination identity.
disassociateOriginationIdentityResponse_originationIdentity :: Lens.Lens' DisassociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
disassociateOriginationIdentityResponse_originationIdentity = Lens.lens (\DisassociateOriginationIdentityResponse' {originationIdentity} -> originationIdentity) (\s@DisassociateOriginationIdentityResponse' {} a -> s {originationIdentity = a} :: DisassociateOriginationIdentityResponse)

-- | The PhoneNumberArn or SenderIdArn of the origination identity.
disassociateOriginationIdentityResponse_originationIdentityArn :: Lens.Lens' DisassociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
disassociateOriginationIdentityResponse_originationIdentityArn = Lens.lens (\DisassociateOriginationIdentityResponse' {originationIdentityArn} -> originationIdentityArn) (\s@DisassociateOriginationIdentityResponse' {} a -> s {originationIdentityArn = a} :: DisassociateOriginationIdentityResponse)

-- | The Amazon Resource Name (ARN) of the pool.
disassociateOriginationIdentityResponse_poolArn :: Lens.Lens' DisassociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
disassociateOriginationIdentityResponse_poolArn = Lens.lens (\DisassociateOriginationIdentityResponse' {poolArn} -> poolArn) (\s@DisassociateOriginationIdentityResponse' {} a -> s {poolArn = a} :: DisassociateOriginationIdentityResponse)

-- | The PoolId of the pool no longer associated with the origination
-- identity.
disassociateOriginationIdentityResponse_poolId :: Lens.Lens' DisassociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
disassociateOriginationIdentityResponse_poolId = Lens.lens (\DisassociateOriginationIdentityResponse' {poolId} -> poolId) (\s@DisassociateOriginationIdentityResponse' {} a -> s {poolId = a} :: DisassociateOriginationIdentityResponse)

-- | The response's http status code.
disassociateOriginationIdentityResponse_httpStatus :: Lens.Lens' DisassociateOriginationIdentityResponse Prelude.Int
disassociateOriginationIdentityResponse_httpStatus = Lens.lens (\DisassociateOriginationIdentityResponse' {httpStatus} -> httpStatus) (\s@DisassociateOriginationIdentityResponse' {} a -> s {httpStatus = a} :: DisassociateOriginationIdentityResponse)

instance
  Prelude.NFData
    DisassociateOriginationIdentityResponse
  where
  rnf DisassociateOriginationIdentityResponse' {..} =
    Prelude.rnf isoCountryCode `Prelude.seq`
      Prelude.rnf originationIdentity `Prelude.seq`
        Prelude.rnf originationIdentityArn `Prelude.seq`
          Prelude.rnf poolArn `Prelude.seq`
            Prelude.rnf poolId `Prelude.seq`
              Prelude.rnf httpStatus
