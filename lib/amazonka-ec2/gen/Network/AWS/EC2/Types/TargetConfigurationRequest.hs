-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetConfigurationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetConfigurationRequest
  ( TargetConfigurationRequest (..),

    -- * Smart constructor
    mkTargetConfigurationRequest,

    -- * Lenses
    tcrInstanceCount,
    tcrOfferingId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the target configuration.
--
-- /See:/ 'mkTargetConfigurationRequest' smart constructor.
data TargetConfigurationRequest = TargetConfigurationRequest'
  { instanceCount ::
      Lude.Maybe Lude.Int,
    offeringId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetConfigurationRequest' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of instances the Covertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
-- * 'offeringId' - The Convertible Reserved Instance offering ID.
mkTargetConfigurationRequest ::
  -- | 'offeringId'
  Lude.Text ->
  TargetConfigurationRequest
mkTargetConfigurationRequest pOfferingId_ =
  TargetConfigurationRequest'
    { instanceCount = Lude.Nothing,
      offeringId = pOfferingId_
    }

-- | The number of instances the Covertible Reserved Instance offering can be applied to. This parameter is reserved and cannot be specified in a request
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrInstanceCount :: Lens.Lens' TargetConfigurationRequest (Lude.Maybe Lude.Int)
tcrInstanceCount = Lens.lens (instanceCount :: TargetConfigurationRequest -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: TargetConfigurationRequest)
{-# DEPRECATED tcrInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The Convertible Reserved Instance offering ID.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcrOfferingId :: Lens.Lens' TargetConfigurationRequest Lude.Text
tcrOfferingId = Lens.lens (offeringId :: TargetConfigurationRequest -> Lude.Text) (\s a -> s {offeringId = a} :: TargetConfigurationRequest)
{-# DEPRECATED tcrOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

instance Lude.ToQuery TargetConfigurationRequest where
  toQuery TargetConfigurationRequest' {..} =
    Lude.mconcat
      [ "InstanceCount" Lude.=: instanceCount,
        "OfferingId" Lude.=: offeringId
      ]
