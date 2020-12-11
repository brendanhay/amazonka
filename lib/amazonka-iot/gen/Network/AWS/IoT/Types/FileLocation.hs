-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.FileLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.FileLocation
  ( FileLocation (..),

    -- * Smart constructor
    mkFileLocation,

    -- * Lenses
    flStream,
    flS3Location,
  )
where

import Network.AWS.IoT.Types.S3Location
import Network.AWS.IoT.Types.Stream
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The location of the OTA update.
--
-- /See:/ 'mkFileLocation' smart constructor.
data FileLocation = FileLocation'
  { stream :: Lude.Maybe Stream,
    s3Location :: Lude.Maybe S3Location
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileLocation' with the minimum fields required to make a request.
--
-- * 's3Location' - The location of the updated firmware in S3.
-- * 'stream' - The stream that contains the OTA update.
mkFileLocation ::
  FileLocation
mkFileLocation =
  FileLocation' {stream = Lude.Nothing, s3Location = Lude.Nothing}

-- | The stream that contains the OTA update.
--
-- /Note:/ Consider using 'stream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flStream :: Lens.Lens' FileLocation (Lude.Maybe Stream)
flStream = Lens.lens (stream :: FileLocation -> Lude.Maybe Stream) (\s a -> s {stream = a} :: FileLocation)
{-# DEPRECATED flStream "Use generic-lens or generic-optics with 'stream' instead." #-}

-- | The location of the updated firmware in S3.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
flS3Location :: Lens.Lens' FileLocation (Lude.Maybe S3Location)
flS3Location = Lens.lens (s3Location :: FileLocation -> Lude.Maybe S3Location) (\s a -> s {s3Location = a} :: FileLocation)
{-# DEPRECATED flS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

instance Lude.FromJSON FileLocation where
  parseJSON =
    Lude.withObject
      "FileLocation"
      ( \x ->
          FileLocation'
            Lude.<$> (x Lude..:? "stream") Lude.<*> (x Lude..:? "s3Location")
      )

instance Lude.ToJSON FileLocation where
  toJSON FileLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stream" Lude..=) Lude.<$> stream,
            ("s3Location" Lude..=) Lude.<$> s3Location
          ]
      )
