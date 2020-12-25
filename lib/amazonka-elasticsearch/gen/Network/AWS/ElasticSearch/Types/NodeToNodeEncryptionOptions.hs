{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
  ( NodeToNodeEncryptionOptions (..),

    -- * Smart constructor
    mkNodeToNodeEncryptionOptions,

    -- * Lenses
    ntneoEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the node-to-node encryption options.
--
-- /See:/ 'mkNodeToNodeEncryptionOptions' smart constructor.
newtype NodeToNodeEncryptionOptions = NodeToNodeEncryptionOptions'
  { -- | Specify true to enable node-to-node encryption.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NodeToNodeEncryptionOptions' value with any optional fields omitted.
mkNodeToNodeEncryptionOptions ::
  NodeToNodeEncryptionOptions
mkNodeToNodeEncryptionOptions =
  NodeToNodeEncryptionOptions' {enabled = Core.Nothing}

-- | Specify true to enable node-to-node encryption.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntneoEnabled :: Lens.Lens' NodeToNodeEncryptionOptions (Core.Maybe Core.Bool)
ntneoEnabled = Lens.field @"enabled"
{-# DEPRECATED ntneoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromJSON NodeToNodeEncryptionOptions where
  toJSON NodeToNodeEncryptionOptions {..} =
    Core.object
      (Core.catMaybes [("Enabled" Core..=) Core.<$> enabled])

instance Core.FromJSON NodeToNodeEncryptionOptions where
  parseJSON =
    Core.withObject "NodeToNodeEncryptionOptions" Core.$
      \x -> NodeToNodeEncryptionOptions' Core.<$> (x Core..:? "Enabled")
