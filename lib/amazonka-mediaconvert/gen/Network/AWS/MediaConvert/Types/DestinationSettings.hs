{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DestinationSettings
  ( DestinationSettings (..),

    -- * Smart constructor
    mkDestinationSettings,

    -- * Lenses
    dsS3Settings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.S3DestinationSettings
import qualified Network.AWS.Prelude as Lude

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /See:/ 'mkDestinationSettings' smart constructor.
newtype DestinationSettings = DestinationSettings'
  { s3Settings ::
      Lude.Maybe S3DestinationSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DestinationSettings' with the minimum fields required to make a request.
--
-- * 's3Settings' - Settings associated with S3 destination
mkDestinationSettings ::
  DestinationSettings
mkDestinationSettings =
  DestinationSettings' {s3Settings = Lude.Nothing}

-- | Settings associated with S3 destination
--
-- /Note:/ Consider using 's3Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsS3Settings :: Lens.Lens' DestinationSettings (Lude.Maybe S3DestinationSettings)
dsS3Settings = Lens.lens (s3Settings :: DestinationSettings -> Lude.Maybe S3DestinationSettings) (\s a -> s {s3Settings = a} :: DestinationSettings)
{-# DEPRECATED dsS3Settings "Use generic-lens or generic-optics with 's3Settings' instead." #-}

instance Lude.FromJSON DestinationSettings where
  parseJSON =
    Lude.withObject
      "DestinationSettings"
      (\x -> DestinationSettings' Lude.<$> (x Lude..:? "s3Settings"))

instance Lude.ToJSON DestinationSettings where
  toJSON DestinationSettings' {..} =
    Lude.object
      (Lude.catMaybes [("s3Settings" Lude..=) Lude.<$> s3Settings])
