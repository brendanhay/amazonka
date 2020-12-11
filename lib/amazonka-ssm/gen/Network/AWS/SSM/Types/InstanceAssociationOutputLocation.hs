-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociationOutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationOutputLocation
  ( InstanceAssociationOutputLocation (..),

    -- * Smart constructor
    mkInstanceAssociationOutputLocation,

    -- * Lenses
    iaolS3Location,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.S3OutputLocation

-- | An S3 bucket where you want to store the results of this request.
--
-- /See:/ 'mkInstanceAssociationOutputLocation' smart constructor.
newtype InstanceAssociationOutputLocation = InstanceAssociationOutputLocation'
  { s3Location ::
      Lude.Maybe
        S3OutputLocation
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceAssociationOutputLocation' with the minimum fields required to make a request.
--
-- * 's3Location' - An S3 bucket where you want to store the results of this request.
mkInstanceAssociationOutputLocation ::
  InstanceAssociationOutputLocation
mkInstanceAssociationOutputLocation =
  InstanceAssociationOutputLocation' {s3Location = Lude.Nothing}

-- | An S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaolS3Location :: Lens.Lens' InstanceAssociationOutputLocation (Lude.Maybe S3OutputLocation)
iaolS3Location = Lens.lens (s3Location :: InstanceAssociationOutputLocation -> Lude.Maybe S3OutputLocation) (\s a -> s {s3Location = a} :: InstanceAssociationOutputLocation)
{-# DEPRECATED iaolS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

instance Lude.FromJSON InstanceAssociationOutputLocation where
  parseJSON =
    Lude.withObject
      "InstanceAssociationOutputLocation"
      ( \x ->
          InstanceAssociationOutputLocation'
            Lude.<$> (x Lude..:? "S3Location")
      )

instance Lude.ToJSON InstanceAssociationOutputLocation where
  toJSON InstanceAssociationOutputLocation' {..} =
    Lude.object
      (Lude.catMaybes [("S3Location" Lude..=) Lude.<$> s3Location])
