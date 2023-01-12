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
-- Module      : Amazonka.Connect.Types.EventBridgeActionDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EventBridgeActionDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The EventBridge action definition.
--
-- /See:/ 'newEventBridgeActionDefinition' smart constructor.
data EventBridgeActionDefinition = EventBridgeActionDefinition'
  { -- | The name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventBridgeActionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'eventBridgeActionDefinition_name' - The name.
newEventBridgeActionDefinition ::
  -- | 'name'
  Prelude.Text ->
  EventBridgeActionDefinition
newEventBridgeActionDefinition pName_ =
  EventBridgeActionDefinition' {name = pName_}

-- | The name.
eventBridgeActionDefinition_name :: Lens.Lens' EventBridgeActionDefinition Prelude.Text
eventBridgeActionDefinition_name = Lens.lens (\EventBridgeActionDefinition' {name} -> name) (\s@EventBridgeActionDefinition' {} a -> s {name = a} :: EventBridgeActionDefinition)

instance Data.FromJSON EventBridgeActionDefinition where
  parseJSON =
    Data.withObject
      "EventBridgeActionDefinition"
      ( \x ->
          EventBridgeActionDefinition'
            Prelude.<$> (x Data..: "Name")
      )

instance Prelude.Hashable EventBridgeActionDefinition where
  hashWithSalt _salt EventBridgeActionDefinition' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData EventBridgeActionDefinition where
  rnf EventBridgeActionDefinition' {..} =
    Prelude.rnf name

instance Data.ToJSON EventBridgeActionDefinition where
  toJSON EventBridgeActionDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )
