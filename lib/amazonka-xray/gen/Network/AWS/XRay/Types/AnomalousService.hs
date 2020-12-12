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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.ServiceId

-- | The service within the service graph that has anomalously high fault rates.
--
-- /See:/ 'mkAnomalousService' smart constructor.
newtype AnomalousService = AnomalousService'
  { serviceId ::
      Lude.Maybe ServiceId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnomalousService' with the minimum fields required to make a request.
--
-- * 'serviceId' - Undocumented field.
mkAnomalousService ::
  AnomalousService
mkAnomalousService = AnomalousService' {serviceId = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asServiceId :: Lens.Lens' AnomalousService (Lude.Maybe ServiceId)
asServiceId = Lens.lens (serviceId :: AnomalousService -> Lude.Maybe ServiceId) (\s a -> s {serviceId = a} :: AnomalousService)
{-# DEPRECATED asServiceId "Use generic-lens or generic-optics with 'serviceId' instead." #-}

instance Lude.FromJSON AnomalousService where
  parseJSON =
    Lude.withObject
      "AnomalousService"
      (\x -> AnomalousService' Lude.<$> (x Lude..:? "ServiceId"))
