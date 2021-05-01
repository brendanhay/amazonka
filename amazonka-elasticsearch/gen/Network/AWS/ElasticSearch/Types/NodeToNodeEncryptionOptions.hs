{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the node-to-node encryption options.
--
-- /See:/ 'newNodeToNodeEncryptionOptions' smart constructor.
data NodeToNodeEncryptionOptions = NodeToNodeEncryptionOptions'
  { -- | Specify true to enable node-to-node encryption.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Specify true to enable node-to-node encryption.
nodeToNodeEncryptionOptions_enabled :: Lens.Lens' NodeToNodeEncryptionOptions (Prelude.Maybe Prelude.Bool)
nodeToNodeEncryptionOptions_enabled = Lens.lens (\NodeToNodeEncryptionOptions' {enabled} -> enabled) (\s@NodeToNodeEncryptionOptions' {} a -> s {enabled = a} :: NodeToNodeEncryptionOptions)

instance Prelude.FromJSON NodeToNodeEncryptionOptions where
  parseJSON =
    Prelude.withObject
      "NodeToNodeEncryptionOptions"
      ( \x ->
          NodeToNodeEncryptionOptions'
            Prelude.<$> (x Prelude..:? "Enabled")
      )

instance Prelude.Hashable NodeToNodeEncryptionOptions

instance Prelude.NFData NodeToNodeEncryptionOptions

instance Prelude.ToJSON NodeToNodeEncryptionOptions where
  toJSON NodeToNodeEncryptionOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Enabled" Prelude..=) Prelude.<$> enabled]
      )
