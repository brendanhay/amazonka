-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.BillingGroupMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BillingGroupMetadata
  ( BillingGroupMetadata (..),

    -- * Smart constructor
    mkBillingGroupMetadata,

    -- * Lenses
    bgmCreationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Additional information about the billing group.
--
-- /See:/ 'mkBillingGroupMetadata' smart constructor.
newtype BillingGroupMetadata = BillingGroupMetadata'
  { creationDate ::
      Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BillingGroupMetadata' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date the billing group was created.
mkBillingGroupMetadata ::
  BillingGroupMetadata
mkBillingGroupMetadata =
  BillingGroupMetadata' {creationDate = Lude.Nothing}

-- | The date the billing group was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgmCreationDate :: Lens.Lens' BillingGroupMetadata (Lude.Maybe Lude.Timestamp)
bgmCreationDate = Lens.lens (creationDate :: BillingGroupMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: BillingGroupMetadata)
{-# DEPRECATED bgmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON BillingGroupMetadata where
  parseJSON =
    Lude.withObject
      "BillingGroupMetadata"
      (\x -> BillingGroupMetadata' Lude.<$> (x Lude..:? "creationDate"))
