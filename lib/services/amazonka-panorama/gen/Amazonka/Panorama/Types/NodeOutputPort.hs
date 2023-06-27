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
-- Module      : Amazonka.Panorama.Types.NodeOutputPort
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NodeOutputPort where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.PortType
import qualified Amazonka.Prelude as Prelude

-- | A node output port.
--
-- /See:/ 'newNodeOutputPort' smart constructor.
data NodeOutputPort = NodeOutputPort'
  { -- | The output port\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The output port\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The output port\'s type.
    type' :: Prelude.Maybe PortType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeOutputPort' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'nodeOutputPort_description' - The output port\'s description.
--
-- 'name', 'nodeOutputPort_name' - The output port\'s name.
--
-- 'type'', 'nodeOutputPort_type' - The output port\'s type.
newNodeOutputPort ::
  NodeOutputPort
newNodeOutputPort =
  NodeOutputPort'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The output port\'s description.
nodeOutputPort_description :: Lens.Lens' NodeOutputPort (Prelude.Maybe Prelude.Text)
nodeOutputPort_description = Lens.lens (\NodeOutputPort' {description} -> description) (\s@NodeOutputPort' {} a -> s {description = a} :: NodeOutputPort)

-- | The output port\'s name.
nodeOutputPort_name :: Lens.Lens' NodeOutputPort (Prelude.Maybe Prelude.Text)
nodeOutputPort_name = Lens.lens (\NodeOutputPort' {name} -> name) (\s@NodeOutputPort' {} a -> s {name = a} :: NodeOutputPort)

-- | The output port\'s type.
nodeOutputPort_type :: Lens.Lens' NodeOutputPort (Prelude.Maybe PortType)
nodeOutputPort_type = Lens.lens (\NodeOutputPort' {type'} -> type') (\s@NodeOutputPort' {} a -> s {type' = a} :: NodeOutputPort)

instance Data.FromJSON NodeOutputPort where
  parseJSON =
    Data.withObject
      "NodeOutputPort"
      ( \x ->
          NodeOutputPort'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable NodeOutputPort where
  hashWithSalt _salt NodeOutputPort' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData NodeOutputPort where
  rnf NodeOutputPort' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
