-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Destination
  ( Destination (..),

    -- * Smart constructor
    mkDestination,

    -- * Lenses
    dS3Destination,
  )
where

import Network.AWS.IoT.Types.S3Destination
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the location of the updated firmware.
--
-- /See:/ 'mkDestination' smart constructor.
newtype Destination = Destination'
  { s3Destination ::
      Lude.Maybe S3Destination
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- * 's3Destination' - Describes the location in S3 of the updated firmware.
mkDestination ::
  Destination
mkDestination = Destination' {s3Destination = Lude.Nothing}

-- | Describes the location in S3 of the updated firmware.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dS3Destination :: Lens.Lens' Destination (Lude.Maybe S3Destination)
dS3Destination = Lens.lens (s3Destination :: Destination -> Lude.Maybe S3Destination) (\s a -> s {s3Destination = a} :: Destination)
{-# DEPRECATED dS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

instance Lude.FromJSON Destination where
  parseJSON =
    Lude.withObject
      "Destination"
      (\x -> Destination' Lude.<$> (x Lude..:? "s3Destination"))

instance Lude.ToJSON Destination where
  toJSON Destination' {..} =
    Lude.object
      (Lude.catMaybes [("s3Destination" Lude..=) Lude.<$> s3Destination])
