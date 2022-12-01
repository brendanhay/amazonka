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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ComponentChild
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ComponentChild where

import Amazonka.AmplifyUiBuilder.Types.ComponentEvent
import Amazonka.AmplifyUiBuilder.Types.ComponentProperty
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A nested UI configuration within a parent @Component@.
--
-- /See:/ 'newComponentChild' smart constructor.
data ComponentChild = ComponentChild'
  { -- | The unique ID of the child component in its original source system, such
    -- as Figma.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The list of @ComponentChild@ instances for this component.
    children :: Prelude.Maybe [ComponentChild],
    -- | Describes the events that can be raised on the child component. Use for
    -- the workflow feature in Amplify Studio that allows you to bind events
    -- and actions to components.
    events :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentEvent),
    -- | The type of the child component.
    componentType :: Prelude.Text,
    -- | The name of the child component.
    name :: Prelude.Text,
    -- | Describes the properties of the child component. You can\'t specify
    -- @tags@ as a valid property for @properties@.
    properties :: Prelude.HashMap Prelude.Text ComponentProperty
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentChild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceId', 'componentChild_sourceId' - The unique ID of the child component in its original source system, such
-- as Figma.
--
-- 'children', 'componentChild_children' - The list of @ComponentChild@ instances for this component.
--
-- 'events', 'componentChild_events' - Describes the events that can be raised on the child component. Use for
-- the workflow feature in Amplify Studio that allows you to bind events
-- and actions to components.
--
-- 'componentType', 'componentChild_componentType' - The type of the child component.
--
-- 'name', 'componentChild_name' - The name of the child component.
--
-- 'properties', 'componentChild_properties' - Describes the properties of the child component. You can\'t specify
-- @tags@ as a valid property for @properties@.
newComponentChild ::
  -- | 'componentType'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ComponentChild
newComponentChild pComponentType_ pName_ =
  ComponentChild'
    { sourceId = Prelude.Nothing,
      children = Prelude.Nothing,
      events = Prelude.Nothing,
      componentType = pComponentType_,
      name = pName_,
      properties = Prelude.mempty
    }

-- | The unique ID of the child component in its original source system, such
-- as Figma.
componentChild_sourceId :: Lens.Lens' ComponentChild (Prelude.Maybe Prelude.Text)
componentChild_sourceId = Lens.lens (\ComponentChild' {sourceId} -> sourceId) (\s@ComponentChild' {} a -> s {sourceId = a} :: ComponentChild)

-- | The list of @ComponentChild@ instances for this component.
componentChild_children :: Lens.Lens' ComponentChild (Prelude.Maybe [ComponentChild])
componentChild_children = Lens.lens (\ComponentChild' {children} -> children) (\s@ComponentChild' {} a -> s {children = a} :: ComponentChild) Prelude.. Lens.mapping Lens.coerced

-- | Describes the events that can be raised on the child component. Use for
-- the workflow feature in Amplify Studio that allows you to bind events
-- and actions to components.
componentChild_events :: Lens.Lens' ComponentChild (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentEvent))
componentChild_events = Lens.lens (\ComponentChild' {events} -> events) (\s@ComponentChild' {} a -> s {events = a} :: ComponentChild) Prelude.. Lens.mapping Lens.coerced

-- | The type of the child component.
componentChild_componentType :: Lens.Lens' ComponentChild Prelude.Text
componentChild_componentType = Lens.lens (\ComponentChild' {componentType} -> componentType) (\s@ComponentChild' {} a -> s {componentType = a} :: ComponentChild)

-- | The name of the child component.
componentChild_name :: Lens.Lens' ComponentChild Prelude.Text
componentChild_name = Lens.lens (\ComponentChild' {name} -> name) (\s@ComponentChild' {} a -> s {name = a} :: ComponentChild)

-- | Describes the properties of the child component. You can\'t specify
-- @tags@ as a valid property for @properties@.
componentChild_properties :: Lens.Lens' ComponentChild (Prelude.HashMap Prelude.Text ComponentProperty)
componentChild_properties = Lens.lens (\ComponentChild' {properties} -> properties) (\s@ComponentChild' {} a -> s {properties = a} :: ComponentChild) Prelude.. Lens.coerced

instance Core.FromJSON ComponentChild where
  parseJSON =
    Core.withObject
      "ComponentChild"
      ( \x ->
          ComponentChild'
            Prelude.<$> (x Core..:? "sourceId")
            Prelude.<*> (x Core..:? "children" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "events" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "componentType")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..:? "properties" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ComponentChild where
  hashWithSalt _salt ComponentChild' {..} =
    _salt `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` children
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` properties

instance Prelude.NFData ComponentChild where
  rnf ComponentChild' {..} =
    Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf children
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf properties

instance Core.ToJSON ComponentChild where
  toJSON ComponentChild' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sourceId" Core..=) Prelude.<$> sourceId,
            ("children" Core..=) Prelude.<$> children,
            ("events" Core..=) Prelude.<$> events,
            Prelude.Just ("componentType" Core..= componentType),
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("properties" Core..= properties)
          ]
      )
