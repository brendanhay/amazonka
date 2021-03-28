{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
  ( VPCDerivedInfoStatus (..)
  -- * Smart constructor
  , mkVPCDerivedInfoStatus
  -- * Lenses
  , vpcdisOptions
  , vpcdisStatus
  ) where

import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.ElasticSearch.Types.VPCDerivedInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of the VPC options for the specified Elasticsearch domain.
--
-- /See:/ 'mkVPCDerivedInfoStatus' smart constructor.
data VPCDerivedInfoStatus = VPCDerivedInfoStatus'
  { options :: Types.VPCDerivedInfo
    -- ^ Specifies the VPC options for the specified Elasticsearch domain.
  , status :: Types.OptionStatus
    -- ^ Specifies the status of the VPC options for the specified Elasticsearch domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VPCDerivedInfoStatus' value with any optional fields omitted.
mkVPCDerivedInfoStatus
    :: Types.VPCDerivedInfo -- ^ 'options'
    -> Types.OptionStatus -- ^ 'status'
    -> VPCDerivedInfoStatus
mkVPCDerivedInfoStatus options status
  = VPCDerivedInfoStatus'{options, status}

-- | Specifies the VPC options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdisOptions :: Lens.Lens' VPCDerivedInfoStatus Types.VPCDerivedInfo
vpcdisOptions = Lens.field @"options"
{-# INLINEABLE vpcdisOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Specifies the status of the VPC options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcdisStatus :: Lens.Lens' VPCDerivedInfoStatus Types.OptionStatus
vpcdisStatus = Lens.field @"status"
{-# INLINEABLE vpcdisStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON VPCDerivedInfoStatus where
        parseJSON
          = Core.withObject "VPCDerivedInfoStatus" Core.$
              \ x ->
                VPCDerivedInfoStatus' Core.<$>
                  (x Core..: "Options") Core.<*> x Core..: "Status"
