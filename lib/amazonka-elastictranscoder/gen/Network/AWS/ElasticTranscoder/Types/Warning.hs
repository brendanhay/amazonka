-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Warning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Warning
  ( Warning (..),

    -- * Smart constructor
    mkWarning,

    -- * Lenses
    wCode,
    wMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
-- /See:/ 'mkWarning' smart constructor.
data Warning = Warning'
  { code :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Warning' with the minimum fields required to make a request.
--
-- * 'code' - The code of the cross-regional warning.
-- * 'message' - The message explaining what resources are in a different region from the pipeline.
mkWarning ::
  Warning
mkWarning = Warning' {code = Lude.Nothing, message = Lude.Nothing}

-- | The code of the cross-regional warning.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wCode :: Lens.Lens' Warning (Lude.Maybe Lude.Text)
wCode = Lens.lens (code :: Warning -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: Warning)
{-# DEPRECATED wCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The message explaining what resources are in a different region from the pipeline.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wMessage :: Lens.Lens' Warning (Lude.Maybe Lude.Text)
wMessage = Lens.lens (message :: Warning -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: Warning)
{-# DEPRECATED wMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON Warning where
  parseJSON =
    Lude.withObject
      "Warning"
      ( \x ->
          Warning'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
