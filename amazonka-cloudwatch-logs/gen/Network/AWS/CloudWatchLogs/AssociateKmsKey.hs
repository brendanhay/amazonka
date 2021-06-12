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
-- Module      : Network.AWS.CloudWatchLogs.AssociateKmsKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified AWS Key Management Service (AWS KMS) customer
-- master key (CMK) with the specified log group.
--
-- Associating an AWS KMS CMK with a log group overrides any existing
-- associations between the log group and a CMK. After a CMK is associated
-- with a log group, all newly ingested data for the log group is encrypted
-- using the CMK. This association is stored as long as the data encrypted
-- with the CMK is still within Amazon CloudWatch Logs. This enables Amazon
-- CloudWatch Logs to decrypt this data whenever it is requested.
--
-- CloudWatch Logs supports only symmetric CMKs. Do not use an associate an
-- asymmetric CMK with your log group. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
--
-- It can take up to 5 minutes for this operation to take effect.
--
-- If you attempt to associate a CMK with a log group but the CMK does not
-- exist or the CMK is disabled, you receive an @InvalidParameterException@
-- error.
module Network.AWS.CloudWatchLogs.AssociateKmsKey
  ( -- * Creating a Request
    AssociateKmsKey (..),
    newAssociateKmsKey,

    -- * Request Lenses
    associateKmsKey_logGroupName,
    associateKmsKey_kmsKeyId,

    -- * Destructuring the Response
    AssociateKmsKeyResponse (..),
    newAssociateKmsKeyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateKmsKey' smart constructor.
data AssociateKmsKey = AssociateKmsKey'
  { -- | The name of the log group.
    logGroupName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
    -- data. This must be a symmetric CMK. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
    kmsKeyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateKmsKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'associateKmsKey_logGroupName' - The name of the log group.
--
-- 'kmsKeyId', 'associateKmsKey_kmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data. This must be a symmetric CMK. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
newAssociateKmsKey ::
  -- | 'logGroupName'
  Core.Text ->
  -- | 'kmsKeyId'
  Core.Text ->
  AssociateKmsKey
newAssociateKmsKey pLogGroupName_ pKmsKeyId_ =
  AssociateKmsKey'
    { logGroupName = pLogGroupName_,
      kmsKeyId = pKmsKeyId_
    }

-- | The name of the log group.
associateKmsKey_logGroupName :: Lens.Lens' AssociateKmsKey Core.Text
associateKmsKey_logGroupName = Lens.lens (\AssociateKmsKey' {logGroupName} -> logGroupName) (\s@AssociateKmsKey' {} a -> s {logGroupName = a} :: AssociateKmsKey)

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data. This must be a symmetric CMK. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
associateKmsKey_kmsKeyId :: Lens.Lens' AssociateKmsKey Core.Text
associateKmsKey_kmsKeyId = Lens.lens (\AssociateKmsKey' {kmsKeyId} -> kmsKeyId) (\s@AssociateKmsKey' {} a -> s {kmsKeyId = a} :: AssociateKmsKey)

instance Core.AWSRequest AssociateKmsKey where
  type
    AWSResponse AssociateKmsKey =
      AssociateKmsKeyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AssociateKmsKeyResponse'

instance Core.Hashable AssociateKmsKey

instance Core.NFData AssociateKmsKey

instance Core.ToHeaders AssociateKmsKey where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.AssociateKmsKey" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateKmsKey where
  toJSON AssociateKmsKey' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("kmsKeyId" Core..= kmsKeyId)
          ]
      )

instance Core.ToPath AssociateKmsKey where
  toPath = Core.const "/"

instance Core.ToQuery AssociateKmsKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateKmsKeyResponse' smart constructor.
data AssociateKmsKeyResponse = AssociateKmsKeyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateKmsKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateKmsKeyResponse ::
  AssociateKmsKeyResponse
newAssociateKmsKeyResponse = AssociateKmsKeyResponse'

instance Core.NFData AssociateKmsKeyResponse
