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
-- Module      : Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus where

import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Status of the node-to-node encryption options for the specified
-- Elasticsearch domain.
--
-- /See:/ 'newNodeToNodeEncryptionOptionsStatus' smart constructor.
data NodeToNodeEncryptionOptionsStatus = NodeToNodeEncryptionOptionsStatus'
  { -- | Specifies the node-to-node encryption options for the specified
    -- Elasticsearch domain.
    options :: NodeToNodeEncryptionOptions,
    -- | Specifies the status of the node-to-node encryption options for the
    -- specified Elasticsearch domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NodeToNodeEncryptionOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'nodeToNodeEncryptionOptionsStatus_options' - Specifies the node-to-node encryption options for the specified
-- Elasticsearch domain.
--
-- 'status', 'nodeToNodeEncryptionOptionsStatus_status' - Specifies the status of the node-to-node encryption options for the
-- specified Elasticsearch domain.
newNodeToNodeEncryptionOptionsStatus ::
  -- | 'options'
  NodeToNodeEncryptionOptions ->
  -- | 'status'
  OptionStatus ->
  NodeToNodeEncryptionOptionsStatus
newNodeToNodeEncryptionOptionsStatus
  pOptions_
  pStatus_ =
    NodeToNodeEncryptionOptionsStatus'
      { options =
          pOptions_,
        status = pStatus_
      }

-- | Specifies the node-to-node encryption options for the specified
-- Elasticsearch domain.
nodeToNodeEncryptionOptionsStatus_options :: Lens.Lens' NodeToNodeEncryptionOptionsStatus NodeToNodeEncryptionOptions
nodeToNodeEncryptionOptionsStatus_options = Lens.lens (\NodeToNodeEncryptionOptionsStatus' {options} -> options) (\s@NodeToNodeEncryptionOptionsStatus' {} a -> s {options = a} :: NodeToNodeEncryptionOptionsStatus)

-- | Specifies the status of the node-to-node encryption options for the
-- specified Elasticsearch domain.
nodeToNodeEncryptionOptionsStatus_status :: Lens.Lens' NodeToNodeEncryptionOptionsStatus OptionStatus
nodeToNodeEncryptionOptionsStatus_status = Lens.lens (\NodeToNodeEncryptionOptionsStatus' {status} -> status) (\s@NodeToNodeEncryptionOptionsStatus' {} a -> s {status = a} :: NodeToNodeEncryptionOptionsStatus)

instance
  Prelude.FromJSON
    NodeToNodeEncryptionOptionsStatus
  where
  parseJSON =
    Prelude.withObject
      "NodeToNodeEncryptionOptionsStatus"
      ( \x ->
          NodeToNodeEncryptionOptionsStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance
  Prelude.Hashable
    NodeToNodeEncryptionOptionsStatus

instance
  Prelude.NFData
    NodeToNodeEncryptionOptionsStatus
