{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuHealth
  ( ElasticGpuHealth (..),

    -- * Smart constructor
    mkElasticGpuHealth,

    -- * Lenses
    eghStatus,
  )
where

import Network.AWS.EC2.Types.ElasticGpuStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the status of an Elastic Graphics accelerator.
--
-- /See:/ 'mkElasticGpuHealth' smart constructor.
newtype ElasticGpuHealth = ElasticGpuHealth'
  { status ::
      Lude.Maybe ElasticGpuStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticGpuHealth' with the minimum fields required to make a request.
--
-- * 'status' - The health status.
mkElasticGpuHealth ::
  ElasticGpuHealth
mkElasticGpuHealth = ElasticGpuHealth' {status = Lude.Nothing}

-- | The health status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eghStatus :: Lens.Lens' ElasticGpuHealth (Lude.Maybe ElasticGpuStatus)
eghStatus = Lens.lens (status :: ElasticGpuHealth -> Lude.Maybe ElasticGpuStatus) (\s a -> s {status = a} :: ElasticGpuHealth)
{-# DEPRECATED eghStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML ElasticGpuHealth where
  parseXML x = ElasticGpuHealth' Lude.<$> (x Lude..@? "status")
