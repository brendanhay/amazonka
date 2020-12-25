{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.AnomalousService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AnomalousService
  ( AnomalousService (..),

    -- * Smart constructor
    mkAnomalousService,

    -- * Lenses
    asServiceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.ServiceId as Types

-- | The service within the service graph that has anomalously high fault rates.
--
-- /See:/ 'mkAnomalousService' smart constructor.
newtype AnomalousService = AnomalousService'
  { serviceId :: Core.Maybe Types.ServiceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AnomalousService' value with any optional fields omitted.
mkAnomalousService ::
  AnomalousService
mkAnomalousService = AnomalousService' {serviceId = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asServiceId :: Lens.Lens' AnomalousService (Core.Maybe Types.ServiceId)
asServiceId = Lens.field @"serviceId"
{-# DEPRECATED asServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

instance Core.FromJSON AnomalousService where
  parseJSON =
    Core.withObject "AnomalousService" Core.$
      \x -> AnomalousService' Core.<$> (x Core..:? "ServiceId")
