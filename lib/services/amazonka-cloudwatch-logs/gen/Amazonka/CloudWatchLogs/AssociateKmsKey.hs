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
-- Module      : Amazonka.CloudWatchLogs.AssociateKmsKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified KMS key with the specified log group.
--
-- Associating a KMS key with a log group overrides any existing
-- associations between the log group and a KMS key. After a KMS key is
-- associated with a log group, all newly ingested data for the log group
-- is encrypted using the KMS key. This association is stored as long as
-- the data encrypted with the KMS keyis still within CloudWatch Logs. This
-- enables CloudWatch Logs to decrypt this data whenever it is requested.
--
-- CloudWatch Logs supports only symmetric KMS keys. Do not use an
-- associate an asymmetric KMS key with your log group. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
--
-- It can take up to 5 minutes for this operation to take effect.
--
-- If you attempt to associate a KMS key with a log group but the KMS key
-- does not exist or the KMS key is disabled, you receive an
-- @InvalidParameterException@ error.
module Amazonka.CloudWatchLogs.AssociateKmsKey
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateKmsKey' smart constructor.
data AssociateKmsKey = AssociateKmsKey'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the KMS key to use when encrypting log
    -- data. This must be a symmetric KMS key. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names>
    -- and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'kmsKeyId', 'associateKmsKey_kmsKeyId' - The Amazon Resource Name (ARN) of the KMS key to use when encrypting log
-- data. This must be a symmetric KMS key. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names>
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

-- | The Amazon Resource Name (ARN) of the KMS key to use when encrypting log
-- data. This must be a symmetric KMS key. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names>
-- and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys>.
associateKmsKey_kmsKeyId :: Lens.Lens' AssociateKmsKey Prelude.Text
associateKmsKey_kmsKeyId = Lens.lens (\AssociateKmsKey' {kmsKeyId} -> kmsKeyId) (\s@AssociateKmsKey' {} a -> s {kmsKeyId = a} :: AssociateKmsKey)

instance Core.AWSRequest AssociateKmsKey where
  type
    AWSResponse AssociateKmsKey =
      AssociateKmsKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull AssociateKmsKeyResponse'

instance Prelude.Hashable AssociateKmsKey where
  hashWithSalt _salt AssociateKmsKey' {..} =
    _salt
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData AssociateKmsKey where
  rnf AssociateKmsKey' {..} =
    Prelude.rnf logGroupName `Prelude.seq`
      Prelude.rnf kmsKeyId

instance Data.ToHeaders AssociateKmsKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.AssociateKmsKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateKmsKey where
  toJSON AssociateKmsKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logGroupName" Data..= logGroupName),
            Prelude.Just ("kmsKeyId" Data..= kmsKeyId)
          ]
      )

instance Data.ToPath AssociateKmsKey where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateKmsKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateKmsKeyResponse' smart constructor.
data AssociateKmsKeyResponse = AssociateKmsKeyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateKmsKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateKmsKeyResponse ::
  AssociateKmsKeyResponse
newAssociateKmsKeyResponse = AssociateKmsKeyResponse'

instance Prelude.NFData AssociateKmsKeyResponse where
  rnf _ = ()
