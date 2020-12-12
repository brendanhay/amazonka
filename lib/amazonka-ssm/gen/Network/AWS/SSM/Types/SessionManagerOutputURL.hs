{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionManagerOutputURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionManagerOutputURL
  ( SessionManagerOutputURL (..),

    -- * Smart constructor
    mkSessionManagerOutputURL,

    -- * Lenses
    smouS3OutputURL,
    smouCloudWatchOutputURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Reserved for future use.
--
-- /See:/ 'mkSessionManagerOutputURL' smart constructor.
data SessionManagerOutputURL = SessionManagerOutputURL'
  { s3OutputURL ::
      Lude.Maybe Lude.Text,
    cloudWatchOutputURL :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SessionManagerOutputURL' with the minimum fields required to make a request.
--
-- * 'cloudWatchOutputURL' - Reserved for future use.
-- * 's3OutputURL' - Reserved for future use.
mkSessionManagerOutputURL ::
  SessionManagerOutputURL
mkSessionManagerOutputURL =
  SessionManagerOutputURL'
    { s3OutputURL = Lude.Nothing,
      cloudWatchOutputURL = Lude.Nothing
    }

-- | Reserved for future use.
--
-- /Note:/ Consider using 's3OutputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smouS3OutputURL :: Lens.Lens' SessionManagerOutputURL (Lude.Maybe Lude.Text)
smouS3OutputURL = Lens.lens (s3OutputURL :: SessionManagerOutputURL -> Lude.Maybe Lude.Text) (\s a -> s {s3OutputURL = a} :: SessionManagerOutputURL)
{-# DEPRECATED smouS3OutputURL "Use generic-lens or generic-optics with 's3OutputURL' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'cloudWatchOutputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smouCloudWatchOutputURL :: Lens.Lens' SessionManagerOutputURL (Lude.Maybe Lude.Text)
smouCloudWatchOutputURL = Lens.lens (cloudWatchOutputURL :: SessionManagerOutputURL -> Lude.Maybe Lude.Text) (\s a -> s {cloudWatchOutputURL = a} :: SessionManagerOutputURL)
{-# DEPRECATED smouCloudWatchOutputURL "Use generic-lens or generic-optics with 'cloudWatchOutputURL' instead." #-}

instance Lude.FromJSON SessionManagerOutputURL where
  parseJSON =
    Lude.withObject
      "SessionManagerOutputURL"
      ( \x ->
          SessionManagerOutputURL'
            Lude.<$> (x Lude..:? "S3OutputUrl")
            Lude.<*> (x Lude..:? "CloudWatchOutputUrl")
      )
