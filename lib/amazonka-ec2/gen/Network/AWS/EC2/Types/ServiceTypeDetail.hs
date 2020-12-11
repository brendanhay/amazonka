-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ServiceTypeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceTypeDetail
  ( ServiceTypeDetail (..),

    -- * Smart constructor
    mkServiceTypeDetail,

    -- * Lenses
    stdServiceType,
  )
where

import Network.AWS.EC2.Types.ServiceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the type of service for a VPC endpoint.
--
-- /See:/ 'mkServiceTypeDetail' smart constructor.
newtype ServiceTypeDetail = ServiceTypeDetail'
  { serviceType ::
      Lude.Maybe ServiceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceTypeDetail' with the minimum fields required to make a request.
--
-- * 'serviceType' - The type of service.
mkServiceTypeDetail ::
  ServiceTypeDetail
mkServiceTypeDetail =
  ServiceTypeDetail' {serviceType = Lude.Nothing}

-- | The type of service.
--
-- /Note:/ Consider using 'serviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdServiceType :: Lens.Lens' ServiceTypeDetail (Lude.Maybe ServiceType)
stdServiceType = Lens.lens (serviceType :: ServiceTypeDetail -> Lude.Maybe ServiceType) (\s a -> s {serviceType = a} :: ServiceTypeDetail)
{-# DEPRECATED stdServiceType "Use generic-lens or generic-optics with 'serviceType' instead." #-}

instance Lude.FromXML ServiceTypeDetail where
  parseXML x = ServiceTypeDetail' Lude.<$> (x Lude..@? "serviceType")
