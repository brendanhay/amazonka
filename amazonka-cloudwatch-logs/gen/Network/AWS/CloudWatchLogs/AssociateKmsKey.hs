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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateKmsKey' smart constructor.
data AssociateKmsKey = AssociateKmsKey'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
    -- data. This must be a symmetric CMK. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  AssociateKmsKey
newAssociateKmsKey pLogGroupName_ pKmsKeyId_ =
  AssociateKmsKey'
    { logGroupName = pLogGroupName_,
      kmsKeyId = pKmsKeyId_
    }

-- | The name of the log group.
associateKmsKey_logGroupName :: Lens.Lens' AssociateKmsKey Prelude.Text
associateKmsKey_logGroupName = Lens.lens (\AssociateKmsKey' {logGroupName} -> logGroupName) (\s@AssociateKmsKey' {} a -> s {logGroupName = a} :: AssociateKmsKey)

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data. This must be a symmetric CMK. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
associateKmsKey_kmsKeyId :: Lens.Lens' AssociateKmsKey Prelude.Text
associateKmsKey_kmsKeyId = Lens.lens (\AssociateKmsKey' {kmsKeyId} -> kmsKeyId) (\s@AssociateKmsKey' {} a -> s {kmsKeyId = a} :: AssociateKmsKey)

instance Prelude.AWSRequest AssociateKmsKey where
  type Rs AssociateKmsKey = AssociateKmsKeyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull AssociateKmsKeyResponse'

instance Prelude.Hashable AssociateKmsKey

instance Prelude.NFData AssociateKmsKey

instance Prelude.ToHeaders AssociateKmsKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Logs_20140328.AssociateKmsKey" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateKmsKey where
  toJSON AssociateKmsKey' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("logGroupName" Prelude..= logGroupName),
            Prelude.Just ("kmsKeyId" Prelude..= kmsKeyId)
          ]
      )

instance Prelude.ToPath AssociateKmsKey where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateKmsKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateKmsKeyResponse' smart constructor.
data AssociateKmsKeyResponse = AssociateKmsKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateKmsKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateKmsKeyResponse ::
  AssociateKmsKeyResponse
newAssociateKmsKeyResponse = AssociateKmsKeyResponse'

instance Prelude.NFData AssociateKmsKeyResponse
