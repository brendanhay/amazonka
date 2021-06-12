{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the node-to-node encryption options.
--
-- /See:/ 'newNodeToNodeEncryptionOptions' smart constructor.
data NodeToNodeEncryptionOptions = NodeToNodeEncryptionOptions'
  { -- | Specify true to enable node-to-node encryption.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeToNodeEncryptionOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'nodeToNodeEncryptionOptions_enabled' - Specify true to enable node-to-node encryption.
newNodeToNodeEncryptionOptions ::
  NodeToNodeEncryptionOptions
newNodeToNodeEncryptionOptions =
  NodeToNodeEncryptionOptions'
    { enabled =
        Core.Nothing
    }

-- | Specify true to enable node-to-node encryption.
nodeToNodeEncryptionOptions_enabled :: Lens.Lens' NodeToNodeEncryptionOptions (Core.Maybe Core.Bool)
nodeToNodeEncryptionOptions_enabled = Lens.lens (\NodeToNodeEncryptionOptions' {enabled} -> enabled) (\s@NodeToNodeEncryptionOptions' {} a -> s {enabled = a} :: NodeToNodeEncryptionOptions)

instance Core.FromJSON NodeToNodeEncryptionOptions where
  parseJSON =
    Core.withObject
      "NodeToNodeEncryptionOptions"
      ( \x ->
          NodeToNodeEncryptionOptions'
            Core.<$> (x Core..:? "Enabled")
      )

instance Core.Hashable NodeToNodeEncryptionOptions

instance Core.NFData NodeToNodeEncryptionOptions

instance Core.ToJSON NodeToNodeEncryptionOptions where
  toJSON NodeToNodeEncryptionOptions' {..} =
    Core.object
      ( Core.catMaybes
          [("Enabled" Core..=) Core.<$> enabled]
      )
