{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiOutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiOutputDataConfig
  ( PiiOutputDataConfig (..),

    -- * Smart constructor
    mkPiiOutputDataConfig,

    -- * Lenses
    podcKMSKeyId,
    podcS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides configuration parameters for the output of PII entity detection jobs.
--
-- /See:/ 'mkPiiOutputDataConfig' smart constructor.
data PiiOutputDataConfig = PiiOutputDataConfig'
  { kmsKeyId ::
      Lude.Maybe Lude.Text,
    s3URI :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PiiOutputDataConfig' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job.
-- * 's3URI' - When you use the @PiiOutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data.
mkPiiOutputDataConfig ::
  -- | 's3URI'
  Lude.Text ->
  PiiOutputDataConfig
mkPiiOutputDataConfig pS3URI_ =
  PiiOutputDataConfig' {kmsKeyId = Lude.Nothing, s3URI = pS3URI_}

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
podcKMSKeyId :: Lens.Lens' PiiOutputDataConfig (Lude.Maybe Lude.Text)
podcKMSKeyId = Lens.lens (kmsKeyId :: PiiOutputDataConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: PiiOutputDataConfig)
{-# DEPRECATED podcKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | When you use the @PiiOutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
podcS3URI :: Lens.Lens' PiiOutputDataConfig Lude.Text
podcS3URI = Lens.lens (s3URI :: PiiOutputDataConfig -> Lude.Text) (\s a -> s {s3URI = a} :: PiiOutputDataConfig)
{-# DEPRECATED podcS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON PiiOutputDataConfig where
  parseJSON =
    Lude.withObject
      "PiiOutputDataConfig"
      ( \x ->
          PiiOutputDataConfig'
            Lude.<$> (x Lude..:? "KmsKeyId") Lude.<*> (x Lude..: "S3Uri")
      )
