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
-- Module      : Amazonka.DAX.Types.NodeTypeSpecificValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.NodeTypeSpecificValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a parameter value that is applicable to a particular node
-- type.
--
-- /See:/ 'newNodeTypeSpecificValue' smart constructor.
data NodeTypeSpecificValue = NodeTypeSpecificValue'
  { -- | A node type to which the parameter value applies.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The parameter value for this node type.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeTypeSpecificValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeType', 'nodeTypeSpecificValue_nodeType' - A node type to which the parameter value applies.
--
-- 'value', 'nodeTypeSpecificValue_value' - The parameter value for this node type.
newNodeTypeSpecificValue ::
  NodeTypeSpecificValue
newNodeTypeSpecificValue =
  NodeTypeSpecificValue'
    { nodeType = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A node type to which the parameter value applies.
nodeTypeSpecificValue_nodeType :: Lens.Lens' NodeTypeSpecificValue (Prelude.Maybe Prelude.Text)
nodeTypeSpecificValue_nodeType = Lens.lens (\NodeTypeSpecificValue' {nodeType} -> nodeType) (\s@NodeTypeSpecificValue' {} a -> s {nodeType = a} :: NodeTypeSpecificValue)

-- | The parameter value for this node type.
nodeTypeSpecificValue_value :: Lens.Lens' NodeTypeSpecificValue (Prelude.Maybe Prelude.Text)
nodeTypeSpecificValue_value = Lens.lens (\NodeTypeSpecificValue' {value} -> value) (\s@NodeTypeSpecificValue' {} a -> s {value = a} :: NodeTypeSpecificValue)

instance Data.FromJSON NodeTypeSpecificValue where
  parseJSON =
    Data.withObject
      "NodeTypeSpecificValue"
      ( \x ->
          NodeTypeSpecificValue'
            Prelude.<$> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable NodeTypeSpecificValue where
  hashWithSalt _salt NodeTypeSpecificValue' {..} =
    _salt
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` value

instance Prelude.NFData NodeTypeSpecificValue where
  rnf NodeTypeSpecificValue' {..} =
    Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf value
