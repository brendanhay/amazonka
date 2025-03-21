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
-- Module      : Amazonka.KMS.VerifyMac
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies the hash-based message authentication code (HMAC) for a
-- specified message, HMAC KMS key, and MAC algorithm. To verify the HMAC,
-- @VerifyMac@ computes an HMAC using the message, HMAC KMS key, and MAC
-- algorithm that you specify, and compares the computed HMAC to the HMAC
-- that you specify. If the HMACs are identical, the verification succeeds;
-- otherwise, it fails. Verification indicates that the message hasn\'t
-- changed since the HMAC was calculated, and the specified key was used to
-- generate and verify the HMAC.
--
-- HMAC KMS keys and the HMAC algorithms that KMS uses conform to industry
-- standards defined in
-- <https://datatracker.ietf.org/doc/html/rfc2104 RFC 2104>.
--
-- This operation is part of KMS support for HMAC KMS keys. For details,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC keys in KMS>
-- in the /Key Management Service Developer Guide/.
--
-- The KMS key that you use for this operation must be in a compatible key
-- state. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html Key states of KMS keys>
-- in the /Key Management Service Developer Guide/.
--
-- __Cross-account use__: Yes. To perform this operation with a KMS key in
-- a different Amazon Web Services account, specify the key ARN or alias
-- ARN in the value of the @KeyId@ parameter.
--
-- __Required permissions__:
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:VerifyMac>
-- (key policy)
--
-- __Related operations__: GenerateMac
module Amazonka.KMS.VerifyMac
  ( -- * Creating a Request
    VerifyMac (..),
    newVerifyMac,

    -- * Request Lenses
    verifyMac_grantTokens,
    verifyMac_message,
    verifyMac_keyId,
    verifyMac_macAlgorithm,
    verifyMac_mac,

    -- * Destructuring the Response
    VerifyMacResponse (..),
    newVerifyMacResponse,

    -- * Response Lenses
    verifyMacResponse_keyId,
    verifyMacResponse_macAlgorithm,
    verifyMacResponse_macValid,
    verifyMacResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newVerifyMac' smart constructor.
data VerifyMac = VerifyMac'
  { -- | A list of grant tokens.
    --
    -- Use a grant token when your permission to call this operation comes from
    -- a new grant that has not yet achieved /eventual consistency/. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
    -- in the /Key Management Service Developer Guide/.
    grantTokens :: Prelude.Maybe [Prelude.Text],
    -- | The message that will be used in the verification. Enter the same
    -- message that was used to generate the HMAC.
    --
    -- GenerateMac and @VerifyMac@ do not provide special handling for message
    -- digests. If you generated an HMAC for a hash digest of a message, you
    -- must verify the HMAC for the same hash digest.
    message :: Data.Sensitive Data.Base64,
    -- | The KMS key that will be used in the verification.
    --
    -- Enter a key ID of the KMS key that was used to generate the HMAC. If you
    -- identify a different KMS key, the @VerifyMac@ operation fails.
    keyId :: Prelude.Text,
    -- | The MAC algorithm that will be used in the verification. Enter the same
    -- MAC algorithm that was used to compute the HMAC. This algorithm must be
    -- supported by the HMAC KMS key identified by the @KeyId@ parameter.
    macAlgorithm :: MacAlgorithmSpec,
    -- | The HMAC to verify. Enter the HMAC that was generated by the GenerateMac
    -- operation when you specified the same message, HMAC KMS key, and MAC
    -- algorithm as the values specified in this request.
    mac :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyMac' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'verifyMac_grantTokens' - A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'message', 'verifyMac_message' - The message that will be used in the verification. Enter the same
-- message that was used to generate the HMAC.
--
-- GenerateMac and @VerifyMac@ do not provide special handling for message
-- digests. If you generated an HMAC for a hash digest of a message, you
-- must verify the HMAC for the same hash digest.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyId', 'verifyMac_keyId' - The KMS key that will be used in the verification.
--
-- Enter a key ID of the KMS key that was used to generate the HMAC. If you
-- identify a different KMS key, the @VerifyMac@ operation fails.
--
-- 'macAlgorithm', 'verifyMac_macAlgorithm' - The MAC algorithm that will be used in the verification. Enter the same
-- MAC algorithm that was used to compute the HMAC. This algorithm must be
-- supported by the HMAC KMS key identified by the @KeyId@ parameter.
--
-- 'mac', 'verifyMac_mac' - The HMAC to verify. Enter the HMAC that was generated by the GenerateMac
-- operation when you specified the same message, HMAC KMS key, and MAC
-- algorithm as the values specified in this request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newVerifyMac ::
  -- | 'message'
  Prelude.ByteString ->
  -- | 'keyId'
  Prelude.Text ->
  -- | 'macAlgorithm'
  MacAlgorithmSpec ->
  -- | 'mac'
  Prelude.ByteString ->
  VerifyMac
newVerifyMac pMessage_ pKeyId_ pMacAlgorithm_ pMac_ =
  VerifyMac'
    { grantTokens = Prelude.Nothing,
      message =
        Data._Sensitive
          Prelude.. Data._Base64
          Lens.# pMessage_,
      keyId = pKeyId_,
      macAlgorithm = pMacAlgorithm_,
      mac = Data._Base64 Lens.# pMac_
    }

-- | A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
verifyMac_grantTokens :: Lens.Lens' VerifyMac (Prelude.Maybe [Prelude.Text])
verifyMac_grantTokens = Lens.lens (\VerifyMac' {grantTokens} -> grantTokens) (\s@VerifyMac' {} a -> s {grantTokens = a} :: VerifyMac) Prelude.. Lens.mapping Lens.coerced

-- | The message that will be used in the verification. Enter the same
-- message that was used to generate the HMAC.
--
-- GenerateMac and @VerifyMac@ do not provide special handling for message
-- digests. If you generated an HMAC for a hash digest of a message, you
-- must verify the HMAC for the same hash digest.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
verifyMac_message :: Lens.Lens' VerifyMac Prelude.ByteString
verifyMac_message = Lens.lens (\VerifyMac' {message} -> message) (\s@VerifyMac' {} a -> s {message = a} :: VerifyMac) Prelude.. Data._Sensitive Prelude.. Data._Base64

-- | The KMS key that will be used in the verification.
--
-- Enter a key ID of the KMS key that was used to generate the HMAC. If you
-- identify a different KMS key, the @VerifyMac@ operation fails.
verifyMac_keyId :: Lens.Lens' VerifyMac Prelude.Text
verifyMac_keyId = Lens.lens (\VerifyMac' {keyId} -> keyId) (\s@VerifyMac' {} a -> s {keyId = a} :: VerifyMac)

-- | The MAC algorithm that will be used in the verification. Enter the same
-- MAC algorithm that was used to compute the HMAC. This algorithm must be
-- supported by the HMAC KMS key identified by the @KeyId@ parameter.
verifyMac_macAlgorithm :: Lens.Lens' VerifyMac MacAlgorithmSpec
verifyMac_macAlgorithm = Lens.lens (\VerifyMac' {macAlgorithm} -> macAlgorithm) (\s@VerifyMac' {} a -> s {macAlgorithm = a} :: VerifyMac)

-- | The HMAC to verify. Enter the HMAC that was generated by the GenerateMac
-- operation when you specified the same message, HMAC KMS key, and MAC
-- algorithm as the values specified in this request.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
verifyMac_mac :: Lens.Lens' VerifyMac Prelude.ByteString
verifyMac_mac = Lens.lens (\VerifyMac' {mac} -> mac) (\s@VerifyMac' {} a -> s {mac = a} :: VerifyMac) Prelude.. Data._Base64

instance Core.AWSRequest VerifyMac where
  type AWSResponse VerifyMac = VerifyMacResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyMacResponse'
            Prelude.<$> (x Data..?> "KeyId")
            Prelude.<*> (x Data..?> "MacAlgorithm")
            Prelude.<*> (x Data..?> "MacValid")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable VerifyMac where
  hashWithSalt _salt VerifyMac' {..} =
    _salt
      `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` macAlgorithm
      `Prelude.hashWithSalt` mac

instance Prelude.NFData VerifyMac where
  rnf VerifyMac' {..} =
    Prelude.rnf grantTokens `Prelude.seq`
      Prelude.rnf message `Prelude.seq`
        Prelude.rnf keyId `Prelude.seq`
          Prelude.rnf macAlgorithm `Prelude.seq`
            Prelude.rnf mac

instance Data.ToHeaders VerifyMac where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("TrentService.VerifyMac" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON VerifyMac where
  toJSON VerifyMac' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Data..=) Prelude.<$> grantTokens,
            Prelude.Just ("Message" Data..= message),
            Prelude.Just ("KeyId" Data..= keyId),
            Prelude.Just ("MacAlgorithm" Data..= macAlgorithm),
            Prelude.Just ("Mac" Data..= mac)
          ]
      )

instance Data.ToPath VerifyMac where
  toPath = Prelude.const "/"

instance Data.ToQuery VerifyMac where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newVerifyMacResponse' smart constructor.
data VerifyMacResponse = VerifyMacResponse'
  { -- | The HMAC KMS key used in the verification.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The MAC algorithm used in the verification.
    macAlgorithm :: Prelude.Maybe MacAlgorithmSpec,
    -- | A Boolean value that indicates whether the HMAC was verified. A value of
    -- @True@ indicates that the HMAC (@Mac@) was generated with the specified
    -- @Message@, HMAC KMS key (@KeyID@) and @MacAlgorithm.@.
    --
    -- If the HMAC is not verified, the @VerifyMac@ operation fails with a
    -- @KMSInvalidMacException@ exception. This exception indicates that one or
    -- more of the inputs changed since the HMAC was computed.
    macValid :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyMacResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'verifyMacResponse_keyId' - The HMAC KMS key used in the verification.
--
-- 'macAlgorithm', 'verifyMacResponse_macAlgorithm' - The MAC algorithm used in the verification.
--
-- 'macValid', 'verifyMacResponse_macValid' - A Boolean value that indicates whether the HMAC was verified. A value of
-- @True@ indicates that the HMAC (@Mac@) was generated with the specified
-- @Message@, HMAC KMS key (@KeyID@) and @MacAlgorithm.@.
--
-- If the HMAC is not verified, the @VerifyMac@ operation fails with a
-- @KMSInvalidMacException@ exception. This exception indicates that one or
-- more of the inputs changed since the HMAC was computed.
--
-- 'httpStatus', 'verifyMacResponse_httpStatus' - The response's http status code.
newVerifyMacResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  VerifyMacResponse
newVerifyMacResponse pHttpStatus_ =
  VerifyMacResponse'
    { keyId = Prelude.Nothing,
      macAlgorithm = Prelude.Nothing,
      macValid = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The HMAC KMS key used in the verification.
verifyMacResponse_keyId :: Lens.Lens' VerifyMacResponse (Prelude.Maybe Prelude.Text)
verifyMacResponse_keyId = Lens.lens (\VerifyMacResponse' {keyId} -> keyId) (\s@VerifyMacResponse' {} a -> s {keyId = a} :: VerifyMacResponse)

-- | The MAC algorithm used in the verification.
verifyMacResponse_macAlgorithm :: Lens.Lens' VerifyMacResponse (Prelude.Maybe MacAlgorithmSpec)
verifyMacResponse_macAlgorithm = Lens.lens (\VerifyMacResponse' {macAlgorithm} -> macAlgorithm) (\s@VerifyMacResponse' {} a -> s {macAlgorithm = a} :: VerifyMacResponse)

-- | A Boolean value that indicates whether the HMAC was verified. A value of
-- @True@ indicates that the HMAC (@Mac@) was generated with the specified
-- @Message@, HMAC KMS key (@KeyID@) and @MacAlgorithm.@.
--
-- If the HMAC is not verified, the @VerifyMac@ operation fails with a
-- @KMSInvalidMacException@ exception. This exception indicates that one or
-- more of the inputs changed since the HMAC was computed.
verifyMacResponse_macValid :: Lens.Lens' VerifyMacResponse (Prelude.Maybe Prelude.Bool)
verifyMacResponse_macValid = Lens.lens (\VerifyMacResponse' {macValid} -> macValid) (\s@VerifyMacResponse' {} a -> s {macValid = a} :: VerifyMacResponse)

-- | The response's http status code.
verifyMacResponse_httpStatus :: Lens.Lens' VerifyMacResponse Prelude.Int
verifyMacResponse_httpStatus = Lens.lens (\VerifyMacResponse' {httpStatus} -> httpStatus) (\s@VerifyMacResponse' {} a -> s {httpStatus = a} :: VerifyMacResponse)

instance Prelude.NFData VerifyMacResponse where
  rnf VerifyMacResponse' {..} =
    Prelude.rnf keyId `Prelude.seq`
      Prelude.rnf macAlgorithm `Prelude.seq`
        Prelude.rnf macValid `Prelude.seq`
          Prelude.rnf httpStatus
