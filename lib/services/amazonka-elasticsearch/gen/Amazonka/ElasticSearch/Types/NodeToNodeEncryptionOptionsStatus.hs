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
-- Module      : Amazonka.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Amazonka.ElasticSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Data.FromJSON
    NodeToNodeEncryptionOptionsStatus
  where
  parseJSON =
    Data.withObject
      "NodeToNodeEncryptionOptionsStatus"
      ( \x ->
          NodeToNodeEncryptionOptionsStatus'
            Prelude.<$> (x Data..: "Options")
            Prelude.<*> (x Data..: "Status")
      )

instance
  Prelude.Hashable
    NodeToNodeEncryptionOptionsStatus
  where
  hashWithSalt
    _salt
    NodeToNodeEncryptionOptionsStatus' {..} =
      _salt
        `Prelude.hashWithSalt` options
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    NodeToNodeEncryptionOptionsStatus
  where
  rnf NodeToNodeEncryptionOptionsStatus' {..} =
    Prelude.rnf options `Prelude.seq`
      Prelude.rnf status
