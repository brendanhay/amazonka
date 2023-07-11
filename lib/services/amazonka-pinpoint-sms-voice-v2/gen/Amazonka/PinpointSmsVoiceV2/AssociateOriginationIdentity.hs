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
-- Module      : Amazonka.PinpointSmsVoiceV2.AssociateOriginationIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified origination identity with a pool.
--
-- If the origination identity is a phone number and is already associated
-- with another pool, an Error is returned. A sender ID can be associated
-- with multiple pools.
--
-- If the origination identity configuration doesn\'t match the pool\'s
-- configuration, an Error is returned.
module Amazonka.PinpointSmsVoiceV2.AssociateOriginationIdentity
  ( -- * Creating a Request
    AssociateOriginationIdentity (..),
    newAssociateOriginationIdentity,

    -- * Request Lenses
    associateOriginationIdentity_clientToken,
    associateOriginationIdentity_poolId,
    associateOriginationIdentity_originationIdentity,
    associateOriginationIdentity_isoCountryCode,

    -- * Destructuring the Response
    AssociateOriginationIdentityResponse (..),
    newAssociateOriginationIdentityResponse,

    -- * Response Lenses
    associateOriginationIdentityResponse_isoCountryCode,
    associateOriginationIdentityResponse_originationIdentity,
    associateOriginationIdentityResponse_originationIdentityArn,
    associateOriginationIdentityResponse_poolArn,
    associateOriginationIdentityResponse_poolId,
    associateOriginationIdentityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateOriginationIdentity' smart constructor.
data AssociateOriginationIdentity = AssociateOriginationIdentity'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don\'t specify a client token, a
    -- randomly generated token is used for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The pool to update with the new Identity. This value can be either the
    -- PoolId or PoolArn, and you can find these values using DescribePools.
    poolId :: Prelude.Text,
    -- | The origination identity to use, such as PhoneNumberId, PhoneNumberArn,
    -- SenderId, or SenderIdArn. You can use DescribePhoneNumbers to find the
    -- values for PhoneNumberId and PhoneNumberArn, while DescribeSenderIds can
    -- be used to get the values for SenderId and SenderIdArn.
    originationIdentity :: Prelude.Text,
    -- | The new two-character code, in ISO 3166-1 alpha-2 format, for the
    -- country or region of the origination identity.
    isoCountryCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateOriginationIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'associateOriginationIdentity_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
--
-- 'poolId', 'associateOriginationIdentity_poolId' - The pool to update with the new Identity. This value can be either the
-- PoolId or PoolArn, and you can find these values using DescribePools.
--
-- 'originationIdentity', 'associateOriginationIdentity_originationIdentity' - The origination identity to use, such as PhoneNumberId, PhoneNumberArn,
-- SenderId, or SenderIdArn. You can use DescribePhoneNumbers to find the
-- values for PhoneNumberId and PhoneNumberArn, while DescribeSenderIds can
-- be used to get the values for SenderId and SenderIdArn.
--
-- 'isoCountryCode', 'associateOriginationIdentity_isoCountryCode' - The new two-character code, in ISO 3166-1 alpha-2 format, for the
-- country or region of the origination identity.
newAssociateOriginationIdentity ::
  -- | 'poolId'
  Prelude.Text ->
  -- | 'originationIdentity'
  Prelude.Text ->
  -- | 'isoCountryCode'
  Prelude.Text ->
  AssociateOriginationIdentity
newAssociateOriginationIdentity
  pPoolId_
  pOriginationIdentity_
  pIsoCountryCode_ =
    AssociateOriginationIdentity'
      { clientToken =
          Prelude.Nothing,
        poolId = pPoolId_,
        originationIdentity = pOriginationIdentity_,
        isoCountryCode = pIsoCountryCode_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
associateOriginationIdentity_clientToken :: Lens.Lens' AssociateOriginationIdentity (Prelude.Maybe Prelude.Text)
associateOriginationIdentity_clientToken = Lens.lens (\AssociateOriginationIdentity' {clientToken} -> clientToken) (\s@AssociateOriginationIdentity' {} a -> s {clientToken = a} :: AssociateOriginationIdentity)

-- | The pool to update with the new Identity. This value can be either the
-- PoolId or PoolArn, and you can find these values using DescribePools.
associateOriginationIdentity_poolId :: Lens.Lens' AssociateOriginationIdentity Prelude.Text
associateOriginationIdentity_poolId = Lens.lens (\AssociateOriginationIdentity' {poolId} -> poolId) (\s@AssociateOriginationIdentity' {} a -> s {poolId = a} :: AssociateOriginationIdentity)

-- | The origination identity to use, such as PhoneNumberId, PhoneNumberArn,
-- SenderId, or SenderIdArn. You can use DescribePhoneNumbers to find the
-- values for PhoneNumberId and PhoneNumberArn, while DescribeSenderIds can
-- be used to get the values for SenderId and SenderIdArn.
associateOriginationIdentity_originationIdentity :: Lens.Lens' AssociateOriginationIdentity Prelude.Text
associateOriginationIdentity_originationIdentity = Lens.lens (\AssociateOriginationIdentity' {originationIdentity} -> originationIdentity) (\s@AssociateOriginationIdentity' {} a -> s {originationIdentity = a} :: AssociateOriginationIdentity)

-- | The new two-character code, in ISO 3166-1 alpha-2 format, for the
-- country or region of the origination identity.
associateOriginationIdentity_isoCountryCode :: Lens.Lens' AssociateOriginationIdentity Prelude.Text
associateOriginationIdentity_isoCountryCode = Lens.lens (\AssociateOriginationIdentity' {isoCountryCode} -> isoCountryCode) (\s@AssociateOriginationIdentity' {} a -> s {isoCountryCode = a} :: AssociateOriginationIdentity)

instance Core.AWSRequest AssociateOriginationIdentity where
  type
    AWSResponse AssociateOriginationIdentity =
      AssociateOriginationIdentityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateOriginationIdentityResponse'
            Prelude.<$> (x Data..?> "IsoCountryCode")
            Prelude.<*> (x Data..?> "OriginationIdentity")
            Prelude.<*> (x Data..?> "OriginationIdentityArn")
            Prelude.<*> (x Data..?> "PoolArn")
            Prelude.<*> (x Data..?> "PoolId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateOriginationIdentity
  where
  hashWithSalt _salt AssociateOriginationIdentity' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` originationIdentity
      `Prelude.hashWithSalt` isoCountryCode

instance Prelude.NFData AssociateOriginationIdentity where
  rnf AssociateOriginationIdentity' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf originationIdentity
      `Prelude.seq` Prelude.rnf isoCountryCode

instance Data.ToHeaders AssociateOriginationIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.AssociateOriginationIdentity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateOriginationIdentity where
  toJSON AssociateOriginationIdentity' {..} =
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

instance Data.ToPath AssociateOriginationIdentity where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateOriginationIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateOriginationIdentityResponse' smart constructor.
data AssociateOriginationIdentityResponse = AssociateOriginationIdentityResponse'
  { -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region.
    isoCountryCode :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberId or SenderId of the origination identity.
    originationIdentity :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberArn or SenderIdArn of the origination identity.
    originationIdentityArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pool that is now associated with
    -- the origination identity.
    poolArn :: Prelude.Maybe Prelude.Text,
    -- | The PoolId of the pool that is now associated with the origination
    -- identity.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateOriginationIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isoCountryCode', 'associateOriginationIdentityResponse_isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
--
-- 'originationIdentity', 'associateOriginationIdentityResponse_originationIdentity' - The PhoneNumberId or SenderId of the origination identity.
--
-- 'originationIdentityArn', 'associateOriginationIdentityResponse_originationIdentityArn' - The PhoneNumberArn or SenderIdArn of the origination identity.
--
-- 'poolArn', 'associateOriginationIdentityResponse_poolArn' - The Amazon Resource Name (ARN) of the pool that is now associated with
-- the origination identity.
--
-- 'poolId', 'associateOriginationIdentityResponse_poolId' - The PoolId of the pool that is now associated with the origination
-- identity.
--
-- 'httpStatus', 'associateOriginationIdentityResponse_httpStatus' - The response's http status code.
newAssociateOriginationIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateOriginationIdentityResponse
newAssociateOriginationIdentityResponse pHttpStatus_ =
  AssociateOriginationIdentityResponse'
    { isoCountryCode =
        Prelude.Nothing,
      originationIdentity = Prelude.Nothing,
      originationIdentityArn =
        Prelude.Nothing,
      poolArn = Prelude.Nothing,
      poolId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region.
associateOriginationIdentityResponse_isoCountryCode :: Lens.Lens' AssociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
associateOriginationIdentityResponse_isoCountryCode = Lens.lens (\AssociateOriginationIdentityResponse' {isoCountryCode} -> isoCountryCode) (\s@AssociateOriginationIdentityResponse' {} a -> s {isoCountryCode = a} :: AssociateOriginationIdentityResponse)

-- | The PhoneNumberId or SenderId of the origination identity.
associateOriginationIdentityResponse_originationIdentity :: Lens.Lens' AssociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
associateOriginationIdentityResponse_originationIdentity = Lens.lens (\AssociateOriginationIdentityResponse' {originationIdentity} -> originationIdentity) (\s@AssociateOriginationIdentityResponse' {} a -> s {originationIdentity = a} :: AssociateOriginationIdentityResponse)

-- | The PhoneNumberArn or SenderIdArn of the origination identity.
associateOriginationIdentityResponse_originationIdentityArn :: Lens.Lens' AssociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
associateOriginationIdentityResponse_originationIdentityArn = Lens.lens (\AssociateOriginationIdentityResponse' {originationIdentityArn} -> originationIdentityArn) (\s@AssociateOriginationIdentityResponse' {} a -> s {originationIdentityArn = a} :: AssociateOriginationIdentityResponse)

-- | The Amazon Resource Name (ARN) of the pool that is now associated with
-- the origination identity.
associateOriginationIdentityResponse_poolArn :: Lens.Lens' AssociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
associateOriginationIdentityResponse_poolArn = Lens.lens (\AssociateOriginationIdentityResponse' {poolArn} -> poolArn) (\s@AssociateOriginationIdentityResponse' {} a -> s {poolArn = a} :: AssociateOriginationIdentityResponse)

-- | The PoolId of the pool that is now associated with the origination
-- identity.
associateOriginationIdentityResponse_poolId :: Lens.Lens' AssociateOriginationIdentityResponse (Prelude.Maybe Prelude.Text)
associateOriginationIdentityResponse_poolId = Lens.lens (\AssociateOriginationIdentityResponse' {poolId} -> poolId) (\s@AssociateOriginationIdentityResponse' {} a -> s {poolId = a} :: AssociateOriginationIdentityResponse)

-- | The response's http status code.
associateOriginationIdentityResponse_httpStatus :: Lens.Lens' AssociateOriginationIdentityResponse Prelude.Int
associateOriginationIdentityResponse_httpStatus = Lens.lens (\AssociateOriginationIdentityResponse' {httpStatus} -> httpStatus) (\s@AssociateOriginationIdentityResponse' {} a -> s {httpStatus = a} :: AssociateOriginationIdentityResponse)

instance
  Prelude.NFData
    AssociateOriginationIdentityResponse
  where
  rnf AssociateOriginationIdentityResponse' {..} =
    Prelude.rnf isoCountryCode
      `Prelude.seq` Prelude.rnf originationIdentity
      `Prelude.seq` Prelude.rnf originationIdentityArn
      `Prelude.seq` Prelude.rnf poolArn
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf httpStatus
