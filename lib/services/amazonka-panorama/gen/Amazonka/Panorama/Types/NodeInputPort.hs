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
-- Module      : Amazonka.Panorama.Types.NodeInputPort
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NodeInputPort where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.PortType
import qualified Amazonka.Prelude as Prelude

-- | A node input port.
--
-- /See:/ 'newNodeInputPort' smart constructor.
data NodeInputPort = NodeInputPort'
  { -- | The input port\'s default value.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The input port\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The input port\'s max connections.
    maxConnections :: Prelude.Maybe Prelude.Int,
    -- | The input port\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The input port\'s type.
    type' :: Prelude.Maybe PortType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeInputPort' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'nodeInputPort_defaultValue' - The input port\'s default value.
--
-- 'description', 'nodeInputPort_description' - The input port\'s description.
--
-- 'maxConnections', 'nodeInputPort_maxConnections' - The input port\'s max connections.
--
-- 'name', 'nodeInputPort_name' - The input port\'s name.
--
-- 'type'', 'nodeInputPort_type' - The input port\'s type.
newNodeInputPort ::
  NodeInputPort
newNodeInputPort =
  NodeInputPort'
    { defaultValue = Prelude.Nothing,
      description = Prelude.Nothing,
      maxConnections = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The input port\'s default value.
nodeInputPort_defaultValue :: Lens.Lens' NodeInputPort (Prelude.Maybe Prelude.Text)
nodeInputPort_defaultValue = Lens.lens (\NodeInputPort' {defaultValue} -> defaultValue) (\s@NodeInputPort' {} a -> s {defaultValue = a} :: NodeInputPort)

-- | The input port\'s description.
nodeInputPort_description :: Lens.Lens' NodeInputPort (Prelude.Maybe Prelude.Text)
nodeInputPort_description = Lens.lens (\NodeInputPort' {description} -> description) (\s@NodeInputPort' {} a -> s {description = a} :: NodeInputPort)

-- | The input port\'s max connections.
nodeInputPort_maxConnections :: Lens.Lens' NodeInputPort (Prelude.Maybe Prelude.Int)
nodeInputPort_maxConnections = Lens.lens (\NodeInputPort' {maxConnections} -> maxConnections) (\s@NodeInputPort' {} a -> s {maxConnections = a} :: NodeInputPort)

-- | The input port\'s name.
nodeInputPort_name :: Lens.Lens' NodeInputPort (Prelude.Maybe Prelude.Text)
nodeInputPort_name = Lens.lens (\NodeInputPort' {name} -> name) (\s@NodeInputPort' {} a -> s {name = a} :: NodeInputPort)

-- | The input port\'s type.
nodeInputPort_type :: Lens.Lens' NodeInputPort (Prelude.Maybe PortType)
nodeInputPort_type = Lens.lens (\NodeInputPort' {type'} -> type') (\s@NodeInputPort' {} a -> s {type' = a} :: NodeInputPort)

instance Data.FromJSON NodeInputPort where
  parseJSON =
    Data.withObject
      "NodeInputPort"
      ( \x ->
          NodeInputPort'
            Prelude.<$> (x Data..:? "DefaultValue")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "MaxConnections")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable NodeInputPort where
  hashWithSalt _salt NodeInputPort' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` maxConnections
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData NodeInputPort where
  rnf NodeInputPort' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf maxConnections
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
