{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
  ( VPCDerivedInfoStatus (..),

    -- * Smart constructor
    mkVPCDerivedInfoStatus,

    -- * Lenses
    vdisOptions,
    vdisStatus,
  )
where

import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status of the VPC options for the specified Elasticsearch domain.
--
-- /See:/ 'mkVPCDerivedInfoStatus' smart constructor.
data VPCDerivedInfoStatus = VPCDerivedInfoStatus'
  { options ::
      VPCDerivedInfo,
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

-- | Creates a value of 'VPCDerivedInfoStatus' with the minimum fields required to make a request.
--
-- * 'options' - Specifies the VPC options for the specified Elasticsearch domain.
-- * 'status' - Specifies the status of the VPC options for the specified Elasticsearch domain.
mkVPCDerivedInfoStatus ::
  -- | 'options'
  VPCDerivedInfo ->
  -- | 'status'
  OptionStatus ->
  VPCDerivedInfoStatus
mkVPCDerivedInfoStatus pOptions_ pStatus_ =
  VPCDerivedInfoStatus' {options = pOptions_, status = pStatus_}

-- | Specifies the VPC options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdisOptions :: Lens.Lens' VPCDerivedInfoStatus VPCDerivedInfo
vdisOptions = Lens.lens (options :: VPCDerivedInfoStatus -> VPCDerivedInfo) (\s a -> s {options = a} :: VPCDerivedInfoStatus)
{-# DEPRECATED vdisOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Specifies the status of the VPC options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdisStatus :: Lens.Lens' VPCDerivedInfoStatus OptionStatus
vdisStatus = Lens.lens (status :: VPCDerivedInfoStatus -> OptionStatus) (\s a -> s {status = a} :: VPCDerivedInfoStatus)
{-# DEPRECATED vdisStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON VPCDerivedInfoStatus where
  parseJSON =
    Lude.withObject
      "VPCDerivedInfoStatus"
      ( \x ->
          VPCDerivedInfoStatus'
            Lude.<$> (x Lude..: "Options") Lude.<*> (x Lude..: "Status")
      )
