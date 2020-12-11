-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ScalingParametersStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ScalingParametersStatus
  ( ScalingParametersStatus (..),

    -- * Smart constructor
    mkScalingParametersStatus,

    -- * Lenses
    spsOptions,
    spsStatus,
  )
where

import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.CloudSearch.Types.ScalingParameters
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status and configuration of a search domain's scaling parameters.
--
-- /See:/ 'mkScalingParametersStatus' smart constructor.
data ScalingParametersStatus = ScalingParametersStatus'
  { options ::
      ScalingParameters,
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

-- | Creates a value of 'ScalingParametersStatus' with the minimum fields required to make a request.
--
-- * 'options' - Undocumented field.
-- * 'status' - Undocumented field.
mkScalingParametersStatus ::
  -- | 'options'
  ScalingParameters ->
  -- | 'status'
  OptionStatus ->
  ScalingParametersStatus
mkScalingParametersStatus pOptions_ pStatus_ =
  ScalingParametersStatus' {options = pOptions_, status = pStatus_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsOptions :: Lens.Lens' ScalingParametersStatus ScalingParameters
spsOptions = Lens.lens (options :: ScalingParametersStatus -> ScalingParameters) (\s a -> s {options = a} :: ScalingParametersStatus)
{-# DEPRECATED spsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsStatus :: Lens.Lens' ScalingParametersStatus OptionStatus
spsStatus = Lens.lens (status :: ScalingParametersStatus -> OptionStatus) (\s a -> s {status = a} :: ScalingParametersStatus)
{-# DEPRECATED spsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML ScalingParametersStatus where
  parseXML x =
    ScalingParametersStatus'
      Lude.<$> (x Lude..@ "Options") Lude.<*> (x Lude..@ "Status")
