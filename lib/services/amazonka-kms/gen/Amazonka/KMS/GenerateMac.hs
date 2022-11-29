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
-- Module      : Amazonka.KMS.GenerateMac
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a hash-based message authentication code (HMAC) for a message
-- using an HMAC KMS key and a MAC algorithm that the key supports. The MAC
-- algorithm computes the HMAC for the message and the key as described in
-- <https://datatracker.ietf.org/doc/html/rfc2104 RFC 2104>.
--
-- You can use the HMAC that this operation generates with the VerifyMac
-- operation to demonstrate that the original message has not changed.
-- Also, because a secret key is used to create the hash, you can verify
-- that the party that generated the hash has the required secret key. This
-- operation is part of KMS support for HMAC KMS keys. For details, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/hmac.html HMAC keys in KMS>
-- in the //Key Management Service Developer Guide// .
--
-- Best practices recommend that you limit the time during which any
-- signing mechanism, including an HMAC, is effective. This deters an
-- attack where the actor uses a signed message to establish validity
-- repeatedly or long after the message is superseded. HMAC tags do not
-- include a timestamp, but you can include a timestamp in the token or
-- message to help you detect when its time to refresh the HMAC.
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
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html kms:GenerateMac>
-- (key policy)
--
-- __Related operations__: VerifyMac
module Amazonka.KMS.GenerateMac
  ( -- * Creating a Request
    GenerateMac (..),
    newGenerateMac,

    -- * Request Lenses
    generateMac_grantTokens,
    generateMac_message,
    generateMac_keyId,
    generateMac_macAlgorithm,

    -- * Destructuring the Response
    GenerateMacResponse (..),
    newGenerateMacResponse,

    -- * Response Lenses
    generateMacResponse_macAlgorithm,
    generateMacResponse_mac,
    generateMacResponse_keyId,
    generateMacResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGenerateMac' smart constructor.
data GenerateMac = GenerateMac'
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
    -- | The message to be hashed. Specify a message of up to 4,096 bytes.
    --
    -- @GenerateMac@ and VerifyMac do not provide special handling for message
    -- digests. If you generate an HMAC for a hash digest of a message, you
    -- must verify the HMAC of the same hash digest.
    message :: Core.Sensitive Core.Base64,
    -- | The HMAC KMS key to use in the operation. The MAC algorithm computes the
    -- HMAC for the message and the key as described in
    -- <https://datatracker.ietf.org/doc/html/rfc2104 RFC 2104>.
    --
    -- To identify an HMAC KMS key, use the DescribeKey operation and see the
    -- @KeySpec@ field in the response.
    keyId :: Prelude.Text,
    -- | The MAC algorithm used in the operation.
    --
    -- The algorithm must be compatible with the HMAC KMS key that you specify.
    -- To find the MAC algorithms that your HMAC KMS key supports, use the
    -- DescribeKey operation and see the @MacAlgorithms@ field in the
    -- @DescribeKey@ response.
    macAlgorithm :: MacAlgorithmSpec
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateMac' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantTokens', 'generateMac_grantTokens' - A list of grant tokens.
--
-- Use a grant token when your permission to call this operation comes from
-- a new grant that has not yet achieved /eventual consistency/. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grants.html#grant_token Grant token>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/grant-manage.html#using-grant-token Using a grant token>
-- in the /Key Management Service Developer Guide/.
--
-- 'message', 'generateMac_message' - The message to be hashed. Specify a message of up to 4,096 bytes.
--
-- @GenerateMac@ and VerifyMac do not provide special handling for message
-- digests. If you generate an HMAC for a hash digest of a message, you
-- must verify the HMAC of the same hash digest.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyId', 'generateMac_keyId' - The HMAC KMS key to use in the operation. The MAC algorithm computes the
-- HMAC for the message and the key as described in
-- <https://datatracker.ietf.org/doc/html/rfc2104 RFC 2104>.
--
-- To identify an HMAC KMS key, use the DescribeKey operation and see the
-- @KeySpec@ field in the response.
--
-- 'macAlgorithm', 'generateMac_macAlgorithm' - The MAC algorithm used in the operation.
--
-- The algorithm must be compatible with the HMAC KMS key that you specify.
-- To find the MAC algorithms that your HMAC KMS key supports, use the
-- DescribeKey operation and see the @MacAlgorithms@ field in the
-- @DescribeKey@ response.
newGenerateMac ::
  -- | 'message'
  Prelude.ByteString ->
  -- | 'keyId'
  Prelude.Text ->
  -- | 'macAlgorithm'
  MacAlgorithmSpec ->
  GenerateMac
newGenerateMac pMessage_ pKeyId_ pMacAlgorithm_ =
  GenerateMac'
    { grantTokens = Prelude.Nothing,
      message =
        Core._Sensitive Prelude.. Core._Base64
          Lens.# pMessage_,
      keyId = pKeyId_,
      macAlgorithm = pMacAlgorithm_
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
generateMac_grantTokens :: Lens.Lens' GenerateMac (Prelude.Maybe [Prelude.Text])
generateMac_grantTokens = Lens.lens (\GenerateMac' {grantTokens} -> grantTokens) (\s@GenerateMac' {} a -> s {grantTokens = a} :: GenerateMac) Prelude.. Lens.mapping Lens.coerced

-- | The message to be hashed. Specify a message of up to 4,096 bytes.
--
-- @GenerateMac@ and VerifyMac do not provide special handling for message
-- digests. If you generate an HMAC for a hash digest of a message, you
-- must verify the HMAC of the same hash digest.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateMac_message :: Lens.Lens' GenerateMac Prelude.ByteString
generateMac_message = Lens.lens (\GenerateMac' {message} -> message) (\s@GenerateMac' {} a -> s {message = a} :: GenerateMac) Prelude.. Core._Sensitive Prelude.. Core._Base64

-- | The HMAC KMS key to use in the operation. The MAC algorithm computes the
-- HMAC for the message and the key as described in
-- <https://datatracker.ietf.org/doc/html/rfc2104 RFC 2104>.
--
-- To identify an HMAC KMS key, use the DescribeKey operation and see the
-- @KeySpec@ field in the response.
generateMac_keyId :: Lens.Lens' GenerateMac Prelude.Text
generateMac_keyId = Lens.lens (\GenerateMac' {keyId} -> keyId) (\s@GenerateMac' {} a -> s {keyId = a} :: GenerateMac)

-- | The MAC algorithm used in the operation.
--
-- The algorithm must be compatible with the HMAC KMS key that you specify.
-- To find the MAC algorithms that your HMAC KMS key supports, use the
-- DescribeKey operation and see the @MacAlgorithms@ field in the
-- @DescribeKey@ response.
generateMac_macAlgorithm :: Lens.Lens' GenerateMac MacAlgorithmSpec
generateMac_macAlgorithm = Lens.lens (\GenerateMac' {macAlgorithm} -> macAlgorithm) (\s@GenerateMac' {} a -> s {macAlgorithm = a} :: GenerateMac)

instance Core.AWSRequest GenerateMac where
  type AWSResponse GenerateMac = GenerateMacResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateMacResponse'
            Prelude.<$> (x Core..?> "MacAlgorithm")
            Prelude.<*> (x Core..?> "Mac")
            Prelude.<*> (x Core..?> "KeyId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateMac where
  hashWithSalt _salt GenerateMac' {..} =
    _salt `Prelude.hashWithSalt` grantTokens
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` macAlgorithm

instance Prelude.NFData GenerateMac where
  rnf GenerateMac' {..} =
    Prelude.rnf grantTokens
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf macAlgorithm

instance Core.ToHeaders GenerateMac where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("TrentService.GenerateMac" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GenerateMac where
  toJSON GenerateMac' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GrantTokens" Core..=) Prelude.<$> grantTokens,
            Prelude.Just ("Message" Core..= message),
            Prelude.Just ("KeyId" Core..= keyId),
            Prelude.Just ("MacAlgorithm" Core..= macAlgorithm)
          ]
      )

instance Core.ToPath GenerateMac where
  toPath = Prelude.const "/"

instance Core.ToQuery GenerateMac where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateMacResponse' smart constructor.
data GenerateMacResponse = GenerateMacResponse'
  { -- | The MAC algorithm that was used to generate the HMAC.
    macAlgorithm :: Prelude.Maybe MacAlgorithmSpec,
    -- | The hash-based message authentication code (HMAC) for the given message,
    -- key, and MAC algorithm.
    mac :: Prelude.Maybe Core.Base64,
    -- | The HMAC KMS key used in the operation.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateMacResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'macAlgorithm', 'generateMacResponse_macAlgorithm' - The MAC algorithm that was used to generate the HMAC.
--
-- 'mac', 'generateMacResponse_mac' - The hash-based message authentication code (HMAC) for the given message,
-- key, and MAC algorithm.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyId', 'generateMacResponse_keyId' - The HMAC KMS key used in the operation.
--
-- 'httpStatus', 'generateMacResponse_httpStatus' - The response's http status code.
newGenerateMacResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateMacResponse
newGenerateMacResponse pHttpStatus_ =
  GenerateMacResponse'
    { macAlgorithm =
        Prelude.Nothing,
      mac = Prelude.Nothing,
      keyId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The MAC algorithm that was used to generate the HMAC.
generateMacResponse_macAlgorithm :: Lens.Lens' GenerateMacResponse (Prelude.Maybe MacAlgorithmSpec)
generateMacResponse_macAlgorithm = Lens.lens (\GenerateMacResponse' {macAlgorithm} -> macAlgorithm) (\s@GenerateMacResponse' {} a -> s {macAlgorithm = a} :: GenerateMacResponse)

-- | The hash-based message authentication code (HMAC) for the given message,
-- key, and MAC algorithm.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
generateMacResponse_mac :: Lens.Lens' GenerateMacResponse (Prelude.Maybe Prelude.ByteString)
generateMacResponse_mac = Lens.lens (\GenerateMacResponse' {mac} -> mac) (\s@GenerateMacResponse' {} a -> s {mac = a} :: GenerateMacResponse) Prelude.. Lens.mapping Core._Base64

-- | The HMAC KMS key used in the operation.
generateMacResponse_keyId :: Lens.Lens' GenerateMacResponse (Prelude.Maybe Prelude.Text)
generateMacResponse_keyId = Lens.lens (\GenerateMacResponse' {keyId} -> keyId) (\s@GenerateMacResponse' {} a -> s {keyId = a} :: GenerateMacResponse)

-- | The response's http status code.
generateMacResponse_httpStatus :: Lens.Lens' GenerateMacResponse Prelude.Int
generateMacResponse_httpStatus = Lens.lens (\GenerateMacResponse' {httpStatus} -> httpStatus) (\s@GenerateMacResponse' {} a -> s {httpStatus = a} :: GenerateMacResponse)

instance Prelude.NFData GenerateMacResponse where
  rnf GenerateMacResponse' {..} =
    Prelude.rnf macAlgorithm
      `Prelude.seq` Prelude.rnf mac
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf httpStatus
