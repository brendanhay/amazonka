-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ProvisionedCapacityDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ProvisionedCapacityDescription
  ( ProvisionedCapacityDescription (..),

    -- * Smart constructor
    mkProvisionedCapacityDescription,

    -- * Lenses
    pcdCapacityId,
    pcdStartDate,
    pcdExpirationDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The definition for a provisioned capacity unit.
--
-- /See:/ 'mkProvisionedCapacityDescription' smart constructor.
data ProvisionedCapacityDescription = ProvisionedCapacityDescription'
  { capacityId ::
      Lude.Maybe Lude.Text,
    startDate ::
      Lude.Maybe Lude.Text,
    expirationDate ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionedCapacityDescription' with the minimum fields required to make a request.
--
-- * 'capacityId' - The ID that identifies the provisioned capacity unit.
-- * 'expirationDate' - The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
-- * 'startDate' - The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
mkProvisionedCapacityDescription ::
  ProvisionedCapacityDescription
mkProvisionedCapacityDescription =
  ProvisionedCapacityDescription'
    { capacityId = Lude.Nothing,
      startDate = Lude.Nothing,
      expirationDate = Lude.Nothing
    }

-- | The ID that identifies the provisioned capacity unit.
--
-- /Note:/ Consider using 'capacityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdCapacityId :: Lens.Lens' ProvisionedCapacityDescription (Lude.Maybe Lude.Text)
pcdCapacityId = Lens.lens (capacityId :: ProvisionedCapacityDescription -> Lude.Maybe Lude.Text) (\s a -> s {capacityId = a} :: ProvisionedCapacityDescription)
{-# DEPRECATED pcdCapacityId "Use generic-lens or generic-optics with 'capacityId' instead." #-}

-- | The date that the provisioned capacity unit was purchased, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdStartDate :: Lens.Lens' ProvisionedCapacityDescription (Lude.Maybe Lude.Text)
pcdStartDate = Lens.lens (startDate :: ProvisionedCapacityDescription -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: ProvisionedCapacityDescription)
{-# DEPRECATED pcdStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The date that the provisioned capacity unit expires, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcdExpirationDate :: Lens.Lens' ProvisionedCapacityDescription (Lude.Maybe Lude.Text)
pcdExpirationDate = Lens.lens (expirationDate :: ProvisionedCapacityDescription -> Lude.Maybe Lude.Text) (\s a -> s {expirationDate = a} :: ProvisionedCapacityDescription)
{-# DEPRECATED pcdExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

instance Lude.FromJSON ProvisionedCapacityDescription where
  parseJSON =
    Lude.withObject
      "ProvisionedCapacityDescription"
      ( \x ->
          ProvisionedCapacityDescription'
            Lude.<$> (x Lude..:? "CapacityId")
            Lude.<*> (x Lude..:? "StartDate")
            Lude.<*> (x Lude..:? "ExpirationDate")
      )
