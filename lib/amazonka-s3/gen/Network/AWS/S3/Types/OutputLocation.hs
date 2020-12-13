{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OutputLocation
  ( OutputLocation (..),

    -- * Smart constructor
    mkOutputLocation,

    -- * Lenses
    olS3,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.S3Location

-- | Describes the location where the restore job's output is stored.
--
-- /See:/ 'mkOutputLocation' smart constructor.
newtype OutputLocation = OutputLocation'
  { -- | Describes an S3 location that will receive the results of the restore request.
    s3 :: Lude.Maybe S3Location
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputLocation' with the minimum fields required to make a request.
--
-- * 's3' - Describes an S3 location that will receive the results of the restore request.
mkOutputLocation ::
  OutputLocation
mkOutputLocation = OutputLocation' {s3 = Lude.Nothing}

-- | Describes an S3 location that will receive the results of the restore request.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olS3 :: Lens.Lens' OutputLocation (Lude.Maybe S3Location)
olS3 = Lens.lens (s3 :: OutputLocation -> Lude.Maybe S3Location) (\s a -> s {s3 = a} :: OutputLocation)
{-# DEPRECATED olS3 "Use generic-lens or generic-optics with 's3' instead." #-}

instance Lude.ToXML OutputLocation where
  toXML OutputLocation' {..} = Lude.mconcat ["S3" Lude.@= s3]
