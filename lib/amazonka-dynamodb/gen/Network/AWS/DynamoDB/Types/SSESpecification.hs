-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSESpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SSESpecification
  ( SSESpecification (..),

    -- * Smart constructor
    mkSSESpecification,

    -- * Lenses
    ssesEnabled,
    ssesKMSMasterKeyId,
    ssesSSEType,
  )
where

import Network.AWS.DynamoDB.Types.SSEType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the settings used to enable server-side encryption.
--
-- /See:/ 'mkSSESpecification' smart constructor.
data SSESpecification = SSESpecification'
  { enabled ::
      Lude.Maybe Lude.Bool,
    kmsMasterKeyId :: Lude.Maybe Lude.Text,
    sSEType :: Lude.Maybe SSEType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSESpecification' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether server-side encryption is done using an AWS managed CMK or an AWS owned CMK. If enabled (true), server-side encryption type is set to @KMS@ and an AWS managed CMK is used (AWS KMS charges apply). If disabled (false) or not specified, server-side encryption is set to AWS owned CMK.
-- * 'kmsMasterKeyId' - The AWS KMS customer master key (CMK) that should be used for the AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB customer master key alias/aws/dynamodb.
-- * 'sSEType' - Server-side encryption type. The only supported value is:
--
--
--     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
mkSSESpecification ::
  SSESpecification
mkSSESpecification =
  SSESpecification'
    { enabled = Lude.Nothing,
      kmsMasterKeyId = Lude.Nothing,
      sSEType = Lude.Nothing
    }

-- | Indicates whether server-side encryption is done using an AWS managed CMK or an AWS owned CMK. If enabled (true), server-side encryption type is set to @KMS@ and an AWS managed CMK is used (AWS KMS charges apply). If disabled (false) or not specified, server-side encryption is set to AWS owned CMK.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssesEnabled :: Lens.Lens' SSESpecification (Lude.Maybe Lude.Bool)
ssesEnabled = Lens.lens (enabled :: SSESpecification -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: SSESpecification)
{-# DEPRECATED ssesEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The AWS KMS customer master key (CMK) that should be used for the AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB customer master key alias/aws/dynamodb.
--
-- /Note:/ Consider using 'kmsMasterKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssesKMSMasterKeyId :: Lens.Lens' SSESpecification (Lude.Maybe Lude.Text)
ssesKMSMasterKeyId = Lens.lens (kmsMasterKeyId :: SSESpecification -> Lude.Maybe Lude.Text) (\s a -> s {kmsMasterKeyId = a} :: SSESpecification)
{-# DEPRECATED ssesKMSMasterKeyId "Use generic-lens or generic-optics with 'kmsMasterKeyId' instead." #-}

-- | Server-side encryption type. The only supported value is:
--
--
--     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
--
--
--
-- /Note:/ Consider using 'sSEType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssesSSEType :: Lens.Lens' SSESpecification (Lude.Maybe SSEType)
ssesSSEType = Lens.lens (sSEType :: SSESpecification -> Lude.Maybe SSEType) (\s a -> s {sSEType = a} :: SSESpecification)
{-# DEPRECATED ssesSSEType "Use generic-lens or generic-optics with 'sSEType' instead." #-}

instance Lude.ToJSON SSESpecification where
  toJSON SSESpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("KMSMasterKeyId" Lude..=) Lude.<$> kmsMasterKeyId,
            ("SSEType" Lude..=) Lude.<$> sSEType
          ]
      )
