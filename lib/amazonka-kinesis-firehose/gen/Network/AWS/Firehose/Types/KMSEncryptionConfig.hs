{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.KMSEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KMSEncryptionConfig
  ( KMSEncryptionConfig (..),

    -- * Smart constructor
    mkKMSEncryptionConfig,

    -- * Lenses
    kecAWSKMSKeyARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an encryption key for a destination in Amazon S3.
--
-- /See:/ 'mkKMSEncryptionConfig' smart constructor.
newtype KMSEncryptionConfig = KMSEncryptionConfig'
  { -- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the same AWS Region as the destination Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    awsKMSKeyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KMSEncryptionConfig' with the minimum fields required to make a request.
--
-- * 'awsKMSKeyARN' - The Amazon Resource Name (ARN) of the encryption key. Must belong to the same AWS Region as the destination Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
mkKMSEncryptionConfig ::
  -- | 'awsKMSKeyARN'
  Lude.Text ->
  KMSEncryptionConfig
mkKMSEncryptionConfig pAWSKMSKeyARN_ =
  KMSEncryptionConfig' {awsKMSKeyARN = pAWSKMSKeyARN_}

-- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the same AWS Region as the destination Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'awsKMSKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kecAWSKMSKeyARN :: Lens.Lens' KMSEncryptionConfig Lude.Text
kecAWSKMSKeyARN = Lens.lens (awsKMSKeyARN :: KMSEncryptionConfig -> Lude.Text) (\s a -> s {awsKMSKeyARN = a} :: KMSEncryptionConfig)
{-# DEPRECATED kecAWSKMSKeyARN "Use generic-lens or generic-optics with 'awsKMSKeyARN' instead." #-}

instance Lude.FromJSON KMSEncryptionConfig where
  parseJSON =
    Lude.withObject
      "KMSEncryptionConfig"
      (\x -> KMSEncryptionConfig' Lude.<$> (x Lude..: "AWSKMSKeyARN"))

instance Lude.ToJSON KMSEncryptionConfig where
  toJSON KMSEncryptionConfig' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AWSKMSKeyARN" Lude..= awsKMSKeyARN)])
