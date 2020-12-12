{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ElasticIPStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ElasticIPStatus
  ( ElasticIPStatus (..),

    -- * Smart constructor
    mkElasticIPStatus,

    -- * Lenses
    eisStatus,
    eisElasticIP,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the status of the elastic IP (EIP) address.
--
-- /See:/ 'mkElasticIPStatus' smart constructor.
data ElasticIPStatus = ElasticIPStatus'
  { status ::
      Lude.Maybe Lude.Text,
    elasticIP :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticIPStatus' with the minimum fields required to make a request.
--
-- * 'elasticIP' - The elastic IP (EIP) address for the cluster.
-- * 'status' - The status of the elastic IP (EIP) address.
mkElasticIPStatus ::
  ElasticIPStatus
mkElasticIPStatus =
  ElasticIPStatus' {status = Lude.Nothing, elasticIP = Lude.Nothing}

-- | The status of the elastic IP (EIP) address.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eisStatus :: Lens.Lens' ElasticIPStatus (Lude.Maybe Lude.Text)
eisStatus = Lens.lens (status :: ElasticIPStatus -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ElasticIPStatus)
{-# DEPRECATED eisStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The elastic IP (EIP) address for the cluster.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eisElasticIP :: Lens.Lens' ElasticIPStatus (Lude.Maybe Lude.Text)
eisElasticIP = Lens.lens (elasticIP :: ElasticIPStatus -> Lude.Maybe Lude.Text) (\s a -> s {elasticIP = a} :: ElasticIPStatus)
{-# DEPRECATED eisElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

instance Lude.FromXML ElasticIPStatus where
  parseXML x =
    ElasticIPStatus'
      Lude.<$> (x Lude..@? "Status") Lude.<*> (x Lude..@? "ElasticIp")
