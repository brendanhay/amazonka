-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociationOutputURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationOutputURL
  ( InstanceAssociationOutputURL (..),

    -- * Smart constructor
    mkInstanceAssociationOutputURL,

    -- * Lenses
    iaouS3OutputURL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.S3OutputURL

-- | The URL of S3 bucket where you want to store the results of this request.
--
-- /See:/ 'mkInstanceAssociationOutputURL' smart constructor.
newtype InstanceAssociationOutputURL = InstanceAssociationOutputURL'
  { s3OutputURL ::
      Lude.Maybe S3OutputURL
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceAssociationOutputURL' with the minimum fields required to make a request.
--
-- * 's3OutputURL' - The URL of S3 bucket where you want to store the results of this request.
mkInstanceAssociationOutputURL ::
  InstanceAssociationOutputURL
mkInstanceAssociationOutputURL =
  InstanceAssociationOutputURL' {s3OutputURL = Lude.Nothing}

-- | The URL of S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 's3OutputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaouS3OutputURL :: Lens.Lens' InstanceAssociationOutputURL (Lude.Maybe S3OutputURL)
iaouS3OutputURL = Lens.lens (s3OutputURL :: InstanceAssociationOutputURL -> Lude.Maybe S3OutputURL) (\s a -> s {s3OutputURL = a} :: InstanceAssociationOutputURL)
{-# DEPRECATED iaouS3OutputURL "Use generic-lens or generic-optics with 's3OutputURL' instead." #-}

instance Lude.FromJSON InstanceAssociationOutputURL where
  parseJSON =
    Lude.withObject
      "InstanceAssociationOutputURL"
      ( \x ->
          InstanceAssociationOutputURL' Lude.<$> (x Lude..:? "S3OutputUrl")
      )
