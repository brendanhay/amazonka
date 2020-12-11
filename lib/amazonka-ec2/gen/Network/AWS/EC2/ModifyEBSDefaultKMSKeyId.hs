{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyEBSDefaultKMSKeyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the default customer master key (CMK) for EBS encryption by default for your account in this Region.
--
-- AWS creates a unique AWS managed CMK in each Region for use with encryption by default. If you change the default CMK to a symmetric customer managed CMK, it is used instead of the AWS managed CMK. To reset the default CMK to the AWS managed CMK for EBS, use 'ResetEbsDefaultKmsKeyId' . Amazon EBS does not support asymmetric CMKs.
-- If you delete or disable the customer managed CMK that you specified for use with encryption by default, your instances will fail to launch.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyEBSDefaultKMSKeyId
  ( -- * Creating a request
    ModifyEBSDefaultKMSKeyId (..),
    mkModifyEBSDefaultKMSKeyId,

    -- ** Request lenses
    medkkiDryRun,
    medkkiKMSKeyId,

    -- * Destructuring the response
    ModifyEBSDefaultKMSKeyIdResponse (..),
    mkModifyEBSDefaultKMSKeyIdResponse,

    -- ** Response lenses
    medkkirsKMSKeyId,
    medkkirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyEBSDefaultKMSKeyId' smart constructor.
data ModifyEBSDefaultKMSKeyId = ModifyEBSDefaultKMSKeyId'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    kmsKeyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyEBSDefaultKMSKeyId' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'kmsKeyId' - The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ .
--
-- You can specify the CMK using any of the following:
--
--     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Key alias. For example, alias/ExampleAlias.
--
--
--     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias.
--
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
-- Amazon EBS does not support asymmetric CMKs.
mkModifyEBSDefaultKMSKeyId ::
  -- | 'kmsKeyId'
  Lude.Text ->
  ModifyEBSDefaultKMSKeyId
mkModifyEBSDefaultKMSKeyId pKMSKeyId_ =
  ModifyEBSDefaultKMSKeyId'
    { dryRun = Lude.Nothing,
      kmsKeyId = pKMSKeyId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
medkkiDryRun :: Lens.Lens' ModifyEBSDefaultKMSKeyId (Lude.Maybe Lude.Bool)
medkkiDryRun = Lens.lens (dryRun :: ModifyEBSDefaultKMSKeyId -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyEBSDefaultKMSKeyId)
{-# DEPRECATED medkkiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ .
--
-- You can specify the CMK using any of the following:
--
--     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Key alias. For example, alias/ExampleAlias.
--
--
--     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias.
--
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
-- Amazon EBS does not support asymmetric CMKs.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
medkkiKMSKeyId :: Lens.Lens' ModifyEBSDefaultKMSKeyId Lude.Text
medkkiKMSKeyId = Lens.lens (kmsKeyId :: ModifyEBSDefaultKMSKeyId -> Lude.Text) (\s a -> s {kmsKeyId = a} :: ModifyEBSDefaultKMSKeyId)
{-# DEPRECATED medkkiKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Lude.AWSRequest ModifyEBSDefaultKMSKeyId where
  type Rs ModifyEBSDefaultKMSKeyId = ModifyEBSDefaultKMSKeyIdResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyEBSDefaultKMSKeyIdResponse'
            Lude.<$> (x Lude..@? "kmsKeyId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyEBSDefaultKMSKeyId where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyEBSDefaultKMSKeyId where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyEBSDefaultKMSKeyId where
  toQuery ModifyEBSDefaultKMSKeyId' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyEbsDefaultKmsKeyId" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "KmsKeyId" Lude.=: kmsKeyId
      ]

-- | /See:/ 'mkModifyEBSDefaultKMSKeyIdResponse' smart constructor.
data ModifyEBSDefaultKMSKeyIdResponse = ModifyEBSDefaultKMSKeyIdResponse'
  { kmsKeyId ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyEBSDefaultKMSKeyIdResponse' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of the default CMK for encryption by default.
-- * 'responseStatus' - The response status code.
mkModifyEBSDefaultKMSKeyIdResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyEBSDefaultKMSKeyIdResponse
mkModifyEBSDefaultKMSKeyIdResponse pResponseStatus_ =
  ModifyEBSDefaultKMSKeyIdResponse'
    { kmsKeyId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the default CMK for encryption by default.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
medkkirsKMSKeyId :: Lens.Lens' ModifyEBSDefaultKMSKeyIdResponse (Lude.Maybe Lude.Text)
medkkirsKMSKeyId = Lens.lens (kmsKeyId :: ModifyEBSDefaultKMSKeyIdResponse -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: ModifyEBSDefaultKMSKeyIdResponse)
{-# DEPRECATED medkkirsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
medkkirsResponseStatus :: Lens.Lens' ModifyEBSDefaultKMSKeyIdResponse Lude.Int
medkkirsResponseStatus = Lens.lens (responseStatus :: ModifyEBSDefaultKMSKeyIdResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyEBSDefaultKMSKeyIdResponse)
{-# DEPRECATED medkkirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
