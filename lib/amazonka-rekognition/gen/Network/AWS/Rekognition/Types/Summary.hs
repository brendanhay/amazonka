-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Summary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Summary
  ( Summary (..),

    -- * Smart constructor
    mkSummary,

    -- * Lenses
    sS3Object,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.S3Object

-- | The S3 bucket that contains the training summary. The training summary includes aggregated evaluation metrics for the entire testing dataset and metrics for each individual label.
--
-- You get the training summary S3 bucket location by calling 'DescribeProjectVersions' .
--
-- /See:/ 'mkSummary' smart constructor.
newtype Summary = Summary' {s3Object :: Lude.Maybe S3Object}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Summary' with the minimum fields required to make a request.
--
-- * 's3Object' - Undocumented field.
mkSummary ::
  Summary
mkSummary = Summary' {s3Object = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Object' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sS3Object :: Lens.Lens' Summary (Lude.Maybe S3Object)
sS3Object = Lens.lens (s3Object :: Summary -> Lude.Maybe S3Object) (\s a -> s {s3Object = a} :: Summary)
{-# DEPRECATED sS3Object "Use generic-lens or generic-optics with 's3Object' instead." #-}

instance Lude.FromJSON Summary where
  parseJSON =
    Lude.withObject
      "Summary"
      (\x -> Summary' Lude.<$> (x Lude..:? "S3Object"))
