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
-- Module      : Network.AWS.Route53.CreateKeySigningKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new key-signing key (KSK) associated with a hosted zone. You
-- can only have two KSKs per hosted zone.
module Network.AWS.Route53.CreateKeySigningKey
  ( -- * Creating a Request
    CreateKeySigningKey (..),
    newCreateKeySigningKey,

    -- * Request Lenses
    createKeySigningKey_callerReference,
    createKeySigningKey_hostedZoneId,
    createKeySigningKey_keyManagementServiceArn,
    createKeySigningKey_name,
    createKeySigningKey_status,

    -- * Destructuring the Response
    CreateKeySigningKeyResponse (..),
    newCreateKeySigningKeyResponse,

    -- * Response Lenses
    createKeySigningKeyResponse_httpStatus,
    createKeySigningKeyResponse_changeInfo,
    createKeySigningKeyResponse_keySigningKey,
    createKeySigningKeyResponse_location,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | /See:/ 'newCreateKeySigningKey' smart constructor.
data CreateKeySigningKey = CreateKeySigningKey'
  { -- | A unique string that identifies the request.
    callerReference :: Prelude.Text,
    -- | The unique string (ID) used to identify a hosted zone.
    hostedZoneId :: ResourceId,
    -- | The Amazon resource name (ARN) for a customer managed customer master
    -- key (CMK) in AWS Key Management Service (AWS KMS). The
    -- @KeyManagementServiceArn@ must be unique for each key-signing key (KSK)
    -- in a single hosted zone. To see an example of @KeyManagementServiceArn@
    -- that grants the correct permissions for DNSSEC, scroll down to
    -- __Example__.
    --
    -- You must configure the customer managed CMK as follows:
    --
    -- [Status]
    --     Enabled
    --
    -- [Key spec]
    --     ECC_NIST_P256
    --
    -- [Key usage]
    --     Sign and verify
    --
    -- [Key policy]
    --     The key policy must give permission for the following actions:
    --
    --     -   DescribeKey
    --
    --     -   GetPublicKey
    --
    --     -   Sign
    --
    --     The key policy must also include the Amazon Route 53 service in the
    --     principal for your account. Specify the following:
    --
    --     -   @\"Service\": \"api-service.dnssec.route53.aws.internal\"@
    --
    -- For more information about working with a customer managed CMK in AWS
    -- KMS, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS Key Management Service concepts>.
    keyManagementServiceArn :: Prelude.Text,
    -- | A string used to identify a key-signing key (KSK). @Name@ can include
    -- numbers, letters, and underscores (_). @Name@ must be unique for each
    -- key-signing key in the same hosted zone.
    name :: Prelude.Text,
    -- | A string specifying the initial status of the key-signing key (KSK). You
    -- can set the value to @ACTIVE@ or @INACTIVE@.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeySigningKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callerReference', 'createKeySigningKey_callerReference' - A unique string that identifies the request.
--
-- 'hostedZoneId', 'createKeySigningKey_hostedZoneId' - The unique string (ID) used to identify a hosted zone.
--
-- 'keyManagementServiceArn', 'createKeySigningKey_keyManagementServiceArn' - The Amazon resource name (ARN) for a customer managed customer master
-- key (CMK) in AWS Key Management Service (AWS KMS). The
-- @KeyManagementServiceArn@ must be unique for each key-signing key (KSK)
-- in a single hosted zone. To see an example of @KeyManagementServiceArn@
-- that grants the correct permissions for DNSSEC, scroll down to
-- __Example__.
--
-- You must configure the customer managed CMK as follows:
--
-- [Status]
--     Enabled
--
-- [Key spec]
--     ECC_NIST_P256
--
-- [Key usage]
--     Sign and verify
--
-- [Key policy]
--     The key policy must give permission for the following actions:
--
--     -   DescribeKey
--
--     -   GetPublicKey
--
--     -   Sign
--
--     The key policy must also include the Amazon Route 53 service in the
--     principal for your account. Specify the following:
--
--     -   @\"Service\": \"api-service.dnssec.route53.aws.internal\"@
--
-- For more information about working with a customer managed CMK in AWS
-- KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS Key Management Service concepts>.
--
-- 'name', 'createKeySigningKey_name' - A string used to identify a key-signing key (KSK). @Name@ can include
-- numbers, letters, and underscores (_). @Name@ must be unique for each
-- key-signing key in the same hosted zone.
--
-- 'status', 'createKeySigningKey_status' - A string specifying the initial status of the key-signing key (KSK). You
-- can set the value to @ACTIVE@ or @INACTIVE@.
newCreateKeySigningKey ::
  -- | 'callerReference'
  Prelude.Text ->
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'keyManagementServiceArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  CreateKeySigningKey
newCreateKeySigningKey
  pCallerReference_
  pHostedZoneId_
  pKeyManagementServiceArn_
  pName_
  pStatus_ =
    CreateKeySigningKey'
      { callerReference =
          pCallerReference_,
        hostedZoneId = pHostedZoneId_,
        keyManagementServiceArn = pKeyManagementServiceArn_,
        name = pName_,
        status = pStatus_
      }

-- | A unique string that identifies the request.
createKeySigningKey_callerReference :: Lens.Lens' CreateKeySigningKey Prelude.Text
createKeySigningKey_callerReference = Lens.lens (\CreateKeySigningKey' {callerReference} -> callerReference) (\s@CreateKeySigningKey' {} a -> s {callerReference = a} :: CreateKeySigningKey)

-- | The unique string (ID) used to identify a hosted zone.
createKeySigningKey_hostedZoneId :: Lens.Lens' CreateKeySigningKey ResourceId
createKeySigningKey_hostedZoneId = Lens.lens (\CreateKeySigningKey' {hostedZoneId} -> hostedZoneId) (\s@CreateKeySigningKey' {} a -> s {hostedZoneId = a} :: CreateKeySigningKey)

-- | The Amazon resource name (ARN) for a customer managed customer master
-- key (CMK) in AWS Key Management Service (AWS KMS). The
-- @KeyManagementServiceArn@ must be unique for each key-signing key (KSK)
-- in a single hosted zone. To see an example of @KeyManagementServiceArn@
-- that grants the correct permissions for DNSSEC, scroll down to
-- __Example__.
--
-- You must configure the customer managed CMK as follows:
--
-- [Status]
--     Enabled
--
-- [Key spec]
--     ECC_NIST_P256
--
-- [Key usage]
--     Sign and verify
--
-- [Key policy]
--     The key policy must give permission for the following actions:
--
--     -   DescribeKey
--
--     -   GetPublicKey
--
--     -   Sign
--
--     The key policy must also include the Amazon Route 53 service in the
--     principal for your account. Specify the following:
--
--     -   @\"Service\": \"api-service.dnssec.route53.aws.internal\"@
--
-- For more information about working with a customer managed CMK in AWS
-- KMS, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html AWS Key Management Service concepts>.
createKeySigningKey_keyManagementServiceArn :: Lens.Lens' CreateKeySigningKey Prelude.Text
createKeySigningKey_keyManagementServiceArn = Lens.lens (\CreateKeySigningKey' {keyManagementServiceArn} -> keyManagementServiceArn) (\s@CreateKeySigningKey' {} a -> s {keyManagementServiceArn = a} :: CreateKeySigningKey)

-- | A string used to identify a key-signing key (KSK). @Name@ can include
-- numbers, letters, and underscores (_). @Name@ must be unique for each
-- key-signing key in the same hosted zone.
createKeySigningKey_name :: Lens.Lens' CreateKeySigningKey Prelude.Text
createKeySigningKey_name = Lens.lens (\CreateKeySigningKey' {name} -> name) (\s@CreateKeySigningKey' {} a -> s {name = a} :: CreateKeySigningKey)

-- | A string specifying the initial status of the key-signing key (KSK). You
-- can set the value to @ACTIVE@ or @INACTIVE@.
createKeySigningKey_status :: Lens.Lens' CreateKeySigningKey Prelude.Text
createKeySigningKey_status = Lens.lens (\CreateKeySigningKey' {status} -> status) (\s@CreateKeySigningKey' {} a -> s {status = a} :: CreateKeySigningKey)

instance Core.AWSRequest CreateKeySigningKey where
  type
    AWSResponse CreateKeySigningKey =
      CreateKeySigningKeyResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateKeySigningKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "ChangeInfo")
            Prelude.<*> (x Core..@ "KeySigningKey")
            Prelude.<*> (h Core..# "Location")
      )

instance Prelude.Hashable CreateKeySigningKey

instance Prelude.NFData CreateKeySigningKey

instance Core.ToElement CreateKeySigningKey where
  toElement =
    Core.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateKeySigningKeyRequest"

instance Core.ToHeaders CreateKeySigningKey where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateKeySigningKey where
  toPath = Prelude.const "/2013-04-01/keysigningkey"

instance Core.ToQuery CreateKeySigningKey where
  toQuery = Prelude.const Prelude.mempty

instance Core.ToXML CreateKeySigningKey where
  toXML CreateKeySigningKey' {..} =
    Prelude.mconcat
      [ "CallerReference" Core.@= callerReference,
        "HostedZoneId" Core.@= hostedZoneId,
        "KeyManagementServiceArn"
          Core.@= keyManagementServiceArn,
        "Name" Core.@= name,
        "Status" Core.@= status
      ]

-- | /See:/ 'newCreateKeySigningKeyResponse' smart constructor.
data CreateKeySigningKeyResponse = CreateKeySigningKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    changeInfo :: ChangeInfo,
    -- | The key-signing key (KSK) that the request creates.
    keySigningKey :: KeySigningKey,
    -- | The unique URL representing the new key-signing key (KSK).
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeySigningKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createKeySigningKeyResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'createKeySigningKeyResponse_changeInfo' - Undocumented member.
--
-- 'keySigningKey', 'createKeySigningKeyResponse_keySigningKey' - The key-signing key (KSK) that the request creates.
--
-- 'location', 'createKeySigningKeyResponse_location' - The unique URL representing the new key-signing key (KSK).
newCreateKeySigningKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  -- | 'keySigningKey'
  KeySigningKey ->
  -- | 'location'
  Prelude.Text ->
  CreateKeySigningKeyResponse
newCreateKeySigningKeyResponse
  pHttpStatus_
  pChangeInfo_
  pKeySigningKey_
  pLocation_ =
    CreateKeySigningKeyResponse'
      { httpStatus =
          pHttpStatus_,
        changeInfo = pChangeInfo_,
        keySigningKey = pKeySigningKey_,
        location = pLocation_
      }

-- | The response's http status code.
createKeySigningKeyResponse_httpStatus :: Lens.Lens' CreateKeySigningKeyResponse Prelude.Int
createKeySigningKeyResponse_httpStatus = Lens.lens (\CreateKeySigningKeyResponse' {httpStatus} -> httpStatus) (\s@CreateKeySigningKeyResponse' {} a -> s {httpStatus = a} :: CreateKeySigningKeyResponse)

-- | Undocumented member.
createKeySigningKeyResponse_changeInfo :: Lens.Lens' CreateKeySigningKeyResponse ChangeInfo
createKeySigningKeyResponse_changeInfo = Lens.lens (\CreateKeySigningKeyResponse' {changeInfo} -> changeInfo) (\s@CreateKeySigningKeyResponse' {} a -> s {changeInfo = a} :: CreateKeySigningKeyResponse)

-- | The key-signing key (KSK) that the request creates.
createKeySigningKeyResponse_keySigningKey :: Lens.Lens' CreateKeySigningKeyResponse KeySigningKey
createKeySigningKeyResponse_keySigningKey = Lens.lens (\CreateKeySigningKeyResponse' {keySigningKey} -> keySigningKey) (\s@CreateKeySigningKeyResponse' {} a -> s {keySigningKey = a} :: CreateKeySigningKeyResponse)

-- | The unique URL representing the new key-signing key (KSK).
createKeySigningKeyResponse_location :: Lens.Lens' CreateKeySigningKeyResponse Prelude.Text
createKeySigningKeyResponse_location = Lens.lens (\CreateKeySigningKeyResponse' {location} -> location) (\s@CreateKeySigningKeyResponse' {} a -> s {location = a} :: CreateKeySigningKeyResponse)

instance Prelude.NFData CreateKeySigningKeyResponse
