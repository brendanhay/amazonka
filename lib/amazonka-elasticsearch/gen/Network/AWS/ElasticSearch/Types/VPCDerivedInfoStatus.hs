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
    vdisStatus,
    vdisOptions,
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
  { -- | Specifies the status of the VPC options for the specified Elasticsearch domain.
    status :: OptionStatus,
    -- | Specifies the VPC options for the specified Elasticsearch domain.
    options :: VPCDerivedInfo
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCDerivedInfoStatus' with the minimum fields required to make a request.
--
-- * 'status' - Specifies the status of the VPC options for the specified Elasticsearch domain.
-- * 'options' - Specifies the VPC options for the specified Elasticsearch domain.
mkVPCDerivedInfoStatus ::
  -- | 'status'
  OptionStatus ->
  -- | 'options'
  VPCDerivedInfo ->
  VPCDerivedInfoStatus
mkVPCDerivedInfoStatus pStatus_ pOptions_ =
  VPCDerivedInfoStatus' {status = pStatus_, options = pOptions_}

-- | Specifies the status of the VPC options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdisStatus :: Lens.Lens' VPCDerivedInfoStatus OptionStatus
vdisStatus = Lens.lens (status :: VPCDerivedInfoStatus -> OptionStatus) (\s a -> s {status = a} :: VPCDerivedInfoStatus)
{-# DEPRECATED vdisStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the VPC options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdisOptions :: Lens.Lens' VPCDerivedInfoStatus VPCDerivedInfo
vdisOptions = Lens.lens (options :: VPCDerivedInfoStatus -> VPCDerivedInfo) (\s a -> s {options = a} :: VPCDerivedInfoStatus)
{-# DEPRECATED vdisOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromJSON VPCDerivedInfoStatus where
  parseJSON =
    Lude.withObject
      "VPCDerivedInfoStatus"
      ( \x ->
          VPCDerivedInfoStatus'
            Lude.<$> (x Lude..: "Status") Lude.<*> (x Lude..: "Options")
      )
