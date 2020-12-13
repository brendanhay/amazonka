{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.AssociateKMSKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified AWS Key Management Service (AWS KMS) customer master key (CMK) with the specified log group.
--
-- Associating an AWS KMS CMK with a log group overrides any existing associations between the log group and a CMK. After a CMK is associated with a log group, all newly ingested data for the log group is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.
-- /Important:/ CloudWatch Logs supports only symmetric CMKs. Do not use an associate an asymmetric CMK with your log group. For more information, see <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> .
-- It can take up to 5 minutes for this operation to take effect.
-- If you attempt to associate a CMK with a log group but the CMK does not exist or the CMK is disabled, you receive an @InvalidParameterException@ error.
module Network.AWS.CloudWatchLogs.AssociateKMSKey
  ( -- * Creating a request
    AssociateKMSKey (..),
    mkAssociateKMSKey,

    -- ** Request lenses
    akkLogGroupName,
    akkKmsKeyId,

    -- * Destructuring the response
    AssociateKMSKeyResponse (..),
    mkAssociateKMSKeyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateKMSKey' smart constructor.
data AssociateKMSKey = AssociateKMSKey'
  { -- | The name of the log group.
    logGroupName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. This must be a symmetric CMK. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> and <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> .
    kmsKeyId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateKMSKey' with the minimum fields required to make a request.
--
-- * 'logGroupName' - The name of the log group.
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. This must be a symmetric CMK. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> and <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> .
mkAssociateKMSKey ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'kmsKeyId'
  Lude.Text ->
  AssociateKMSKey
mkAssociateKMSKey pLogGroupName_ pKmsKeyId_ =
  AssociateKMSKey'
    { logGroupName = pLogGroupName_,
      kmsKeyId = pKmsKeyId_
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akkLogGroupName :: Lens.Lens' AssociateKMSKey Lude.Text
akkLogGroupName = Lens.lens (logGroupName :: AssociateKMSKey -> Lude.Text) (\s a -> s {logGroupName = a} :: AssociateKMSKey)
{-# DEPRECATED akkLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. This must be a symmetric CMK. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> and <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using Symmetric and Asymmetric Keys> .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akkKmsKeyId :: Lens.Lens' AssociateKMSKey Lude.Text
akkKmsKeyId = Lens.lens (kmsKeyId :: AssociateKMSKey -> Lude.Text) (\s a -> s {kmsKeyId = a} :: AssociateKMSKey)
{-# DEPRECATED akkKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Lude.AWSRequest AssociateKMSKey where
  type Rs AssociateKMSKey = AssociateKMSKeyResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull AssociateKMSKeyResponse'

instance Lude.ToHeaders AssociateKMSKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.AssociateKmsKey" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateKMSKey where
  toJSON AssociateKMSKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("kmsKeyId" Lude..= kmsKeyId)
          ]
      )

instance Lude.ToPath AssociateKMSKey where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateKMSKey where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateKMSKeyResponse' smart constructor.
data AssociateKMSKeyResponse = AssociateKMSKeyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateKMSKeyResponse' with the minimum fields required to make a request.
mkAssociateKMSKeyResponse ::
  AssociateKMSKeyResponse
mkAssociateKMSKeyResponse = AssociateKMSKeyResponse'
