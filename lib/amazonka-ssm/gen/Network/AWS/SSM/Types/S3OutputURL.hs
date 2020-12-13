{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.S3OutputURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.S3OutputURL
  ( S3OutputURL (..),

    -- * Smart constructor
    mkS3OutputURL,

    -- * Lenses
    souOutputURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A URL for the S3 bucket where you want to store the results of this request.
--
-- /See:/ 'mkS3OutputURL' smart constructor.
newtype S3OutputURL = S3OutputURL'
  { -- | A URL for an S3 bucket where you want to store the results of this request.
    outputURL :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3OutputURL' with the minimum fields required to make a request.
--
-- * 'outputURL' - A URL for an S3 bucket where you want to store the results of this request.
mkS3OutputURL ::
  S3OutputURL
mkS3OutputURL = S3OutputURL' {outputURL = Lude.Nothing}

-- | A URL for an S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 'outputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
souOutputURL :: Lens.Lens' S3OutputURL (Lude.Maybe Lude.Text)
souOutputURL = Lens.lens (outputURL :: S3OutputURL -> Lude.Maybe Lude.Text) (\s a -> s {outputURL = a} :: S3OutputURL)
{-# DEPRECATED souOutputURL "Use generic-lens or generic-optics with 'outputURL' instead." #-}

instance Lude.FromJSON S3OutputURL where
  parseJSON =
    Lude.withObject
      "S3OutputURL"
      (\x -> S3OutputURL' Lude.<$> (x Lude..:? "OutputUrl"))
