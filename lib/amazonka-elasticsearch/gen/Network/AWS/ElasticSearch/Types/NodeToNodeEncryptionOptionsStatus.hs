{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
  ( NodeToNodeEncryptionOptionsStatus (..)
  -- * Smart constructor
  , mkNodeToNodeEncryptionOptionsStatus
  -- * Lenses
  , ntneosOptions
  , ntneosStatus
  ) where

import qualified Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions as Types
import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of the node-to-node encryption options for the specified Elasticsearch domain.
--
-- /See:/ 'mkNodeToNodeEncryptionOptionsStatus' smart constructor.
data NodeToNodeEncryptionOptionsStatus = NodeToNodeEncryptionOptionsStatus'
  { options :: Types.NodeToNodeEncryptionOptions
    -- ^ Specifies the node-to-node encryption options for the specified Elasticsearch domain.
  , status :: Types.OptionStatus
    -- ^ Specifies the status of the node-to-node encryption options for the specified Elasticsearch domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'NodeToNodeEncryptionOptionsStatus' value with any optional fields omitted.
mkNodeToNodeEncryptionOptionsStatus
    :: Types.NodeToNodeEncryptionOptions -- ^ 'options'
    -> Types.OptionStatus -- ^ 'status'
    -> NodeToNodeEncryptionOptionsStatus
mkNodeToNodeEncryptionOptionsStatus options status
  = NodeToNodeEncryptionOptionsStatus'{options, status}

-- | Specifies the node-to-node encryption options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntneosOptions :: Lens.Lens' NodeToNodeEncryptionOptionsStatus Types.NodeToNodeEncryptionOptions
ntneosOptions = Lens.field @"options"
{-# INLINEABLE ntneosOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Specifies the status of the node-to-node encryption options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntneosStatus :: Lens.Lens' NodeToNodeEncryptionOptionsStatus Types.OptionStatus
ntneosStatus = Lens.field @"status"
{-# INLINEABLE ntneosStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON NodeToNodeEncryptionOptionsStatus where
        parseJSON
          = Core.withObject "NodeToNodeEncryptionOptionsStatus" Core.$
              \ x ->
                NodeToNodeEncryptionOptionsStatus' Core.<$>
                  (x Core..: "Options") Core.<*> x Core..: "Status"
