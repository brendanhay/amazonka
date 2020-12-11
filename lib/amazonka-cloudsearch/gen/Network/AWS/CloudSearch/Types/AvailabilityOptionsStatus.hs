-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AvailabilityOptionsStatus
  ( AvailabilityOptionsStatus (..),

    -- * Smart constructor
    mkAvailabilityOptionsStatus,

    -- * Lenses
    aosOptions,
    aosStatus,
  )
where

import Network.AWS.CloudSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status and configuration of the domain's availability options.
--
-- /See:/ 'mkAvailabilityOptionsStatus' smart constructor.
data AvailabilityOptionsStatus = AvailabilityOptionsStatus'
  { options ::
      Lude.Bool,
    status :: OptionStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailabilityOptionsStatus' with the minimum fields required to make a request.
--
-- * 'options' - The availability options configured for the domain.
-- * 'status' - Undocumented field.
mkAvailabilityOptionsStatus ::
  -- | 'options'
  Lude.Bool ->
  -- | 'status'
  OptionStatus ->
  AvailabilityOptionsStatus
mkAvailabilityOptionsStatus pOptions_ pStatus_ =
  AvailabilityOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | The availability options configured for the domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosOptions :: Lens.Lens' AvailabilityOptionsStatus Lude.Bool
aosOptions = Lens.lens (options :: AvailabilityOptionsStatus -> Lude.Bool) (\s a -> s {options = a} :: AvailabilityOptionsStatus)
{-# DEPRECATED aosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosStatus :: Lens.Lens' AvailabilityOptionsStatus OptionStatus
aosStatus = Lens.lens (status :: AvailabilityOptionsStatus -> OptionStatus) (\s a -> s {status = a} :: AvailabilityOptionsStatus)
{-# DEPRECATED aosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML AvailabilityOptionsStatus where
  parseXML x =
    AvailabilityOptionsStatus'
      Lude.<$> (x Lude..@ "Options") Lude.<*> (x Lude..@ "Status")
