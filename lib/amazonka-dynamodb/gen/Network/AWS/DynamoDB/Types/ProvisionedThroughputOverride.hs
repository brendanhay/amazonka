{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
  ( ProvisionedThroughputOverride (..),

    -- * Smart constructor
    mkProvisionedThroughputOverride,

    -- * Lenses
    ptoReadCapacityUnits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Replica-specific provisioned throughput settings. If not specified, uses the source table's provisioned throughput settings.
--
-- /See:/ 'mkProvisionedThroughputOverride' smart constructor.
newtype ProvisionedThroughputOverride = ProvisionedThroughputOverride'
  { readCapacityUnits ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisionedThroughputOverride' with the minimum fields required to make a request.
--
-- * 'readCapacityUnits' - Replica-specific read capacity units. If not specified, uses the source table's read capacity settings.
mkProvisionedThroughputOverride ::
  ProvisionedThroughputOverride
mkProvisionedThroughputOverride =
  ProvisionedThroughputOverride' {readCapacityUnits = Lude.Nothing}

-- | Replica-specific read capacity units. If not specified, uses the source table's read capacity settings.
--
-- /Note:/ Consider using 'readCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptoReadCapacityUnits :: Lens.Lens' ProvisionedThroughputOverride (Lude.Maybe Lude.Natural)
ptoReadCapacityUnits = Lens.lens (readCapacityUnits :: ProvisionedThroughputOverride -> Lude.Maybe Lude.Natural) (\s a -> s {readCapacityUnits = a} :: ProvisionedThroughputOverride)
{-# DEPRECATED ptoReadCapacityUnits "Use generic-lens or generic-optics with 'readCapacityUnits' instead." #-}

instance Lude.FromJSON ProvisionedThroughputOverride where
  parseJSON =
    Lude.withObject
      "ProvisionedThroughputOverride"
      ( \x ->
          ProvisionedThroughputOverride'
            Lude.<$> (x Lude..:? "ReadCapacityUnits")
      )

instance Lude.ToJSON ProvisionedThroughputOverride where
  toJSON ProvisionedThroughputOverride' {..} =
    Lude.object
      ( Lude.catMaybes
          [("ReadCapacityUnits" Lude..=) Lude.<$> readCapacityUnits]
      )
