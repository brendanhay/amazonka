{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slS3Key,
    slS3Bucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The bucket and key of an item stored in Amazon S3.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { s3Key :: Lude.Maybe Lude.Text,
    s3Bucket :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- * 's3Bucket' - The Amazon S3 bucket where the data is located.
-- * 's3Key' - The Amazon S3 key where the data is located.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location' {s3Key = Lude.Nothing, s3Bucket = Lude.Nothing}

-- | The Amazon S3 key where the data is located.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slS3Key :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slS3Key = Lens.lens (s3Key :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: S3Location)
{-# DEPRECATED slS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | The Amazon S3 bucket where the data is located.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slS3Bucket :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slS3Bucket = Lens.lens (s3Bucket :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: S3Location)
{-# DEPRECATED slS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.FromXML S3Location where
  parseXML x =
    S3Location'
      Lude.<$> (x Lude..@? "S3Key") Lude.<*> (x Lude..@? "S3Bucket")

instance Lude.ToQuery S3Location where
  toQuery S3Location' {..} =
    Lude.mconcat ["S3Key" Lude.=: s3Key, "S3Bucket" Lude.=: s3Bucket]
