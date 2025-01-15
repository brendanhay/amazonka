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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ActionParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ActionParameters where

import Amazonka.AmplifyUiBuilder.Types.ComponentProperty
import Amazonka.AmplifyUiBuilder.Types.MutationActionSetStateParameter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the event action configuration for an element of a
-- @Component@ or @ComponentChild@. Use for the workflow feature in Amplify
-- Studio that allows you to bind events and actions to components.
-- @ActionParameters@ defines the action that is performed when an event
-- occurs on the component.
--
-- /See:/ 'newActionParameters' smart constructor.
data ActionParameters = ActionParameters'
  { -- | The HTML anchor link to the location to open. Specify this value for a
    -- navigation action.
    anchor :: Prelude.Maybe ComponentProperty,
    -- | A dictionary of key-value pairs mapping Amplify Studio properties to
    -- fields in a data model. Use when the action performs an operation on an
    -- Amplify DataStore model.
    fields :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentProperty),
    -- | Specifies whether the user should be signed out globally. Specify this
    -- value for an auth sign out action.
    global :: Prelude.Maybe ComponentProperty,
    -- | The unique ID of the component that the @ActionParameters@ apply to.
    id :: Prelude.Maybe ComponentProperty,
    -- | The name of the data model. Use when the action performs an operation on
    -- an Amplify DataStore model.
    model :: Prelude.Maybe Prelude.Text,
    -- | A key-value pair that specifies the state property name and its initial
    -- value.
    state :: Prelude.Maybe MutationActionSetStateParameter,
    -- | The element within the same component to modify when the action occurs.
    target :: Prelude.Maybe ComponentProperty,
    -- | The type of navigation action. Valid values are @url@ and @anchor@. This
    -- value is required for a navigation action.
    type' :: Prelude.Maybe ComponentProperty,
    -- | The URL to the location to open. Specify this value for a navigation
    -- action.
    url :: Prelude.Maybe ComponentProperty
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anchor', 'actionParameters_anchor' - The HTML anchor link to the location to open. Specify this value for a
-- navigation action.
--
-- 'fields', 'actionParameters_fields' - A dictionary of key-value pairs mapping Amplify Studio properties to
-- fields in a data model. Use when the action performs an operation on an
-- Amplify DataStore model.
--
-- 'global', 'actionParameters_global' - Specifies whether the user should be signed out globally. Specify this
-- value for an auth sign out action.
--
-- 'id', 'actionParameters_id' - The unique ID of the component that the @ActionParameters@ apply to.
--
-- 'model', 'actionParameters_model' - The name of the data model. Use when the action performs an operation on
-- an Amplify DataStore model.
--
-- 'state', 'actionParameters_state' - A key-value pair that specifies the state property name and its initial
-- value.
--
-- 'target', 'actionParameters_target' - The element within the same component to modify when the action occurs.
--
-- 'type'', 'actionParameters_type' - The type of navigation action. Valid values are @url@ and @anchor@. This
-- value is required for a navigation action.
--
-- 'url', 'actionParameters_url' - The URL to the location to open. Specify this value for a navigation
-- action.
newActionParameters ::
  ActionParameters
newActionParameters =
  ActionParameters'
    { anchor = Prelude.Nothing,
      fields = Prelude.Nothing,
      global = Prelude.Nothing,
      id = Prelude.Nothing,
      model = Prelude.Nothing,
      state = Prelude.Nothing,
      target = Prelude.Nothing,
      type' = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The HTML anchor link to the location to open. Specify this value for a
-- navigation action.
actionParameters_anchor :: Lens.Lens' ActionParameters (Prelude.Maybe ComponentProperty)
actionParameters_anchor = Lens.lens (\ActionParameters' {anchor} -> anchor) (\s@ActionParameters' {} a -> s {anchor = a} :: ActionParameters)

-- | A dictionary of key-value pairs mapping Amplify Studio properties to
-- fields in a data model. Use when the action performs an operation on an
-- Amplify DataStore model.
actionParameters_fields :: Lens.Lens' ActionParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentProperty))
actionParameters_fields = Lens.lens (\ActionParameters' {fields} -> fields) (\s@ActionParameters' {} a -> s {fields = a} :: ActionParameters) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the user should be signed out globally. Specify this
-- value for an auth sign out action.
actionParameters_global :: Lens.Lens' ActionParameters (Prelude.Maybe ComponentProperty)
actionParameters_global = Lens.lens (\ActionParameters' {global} -> global) (\s@ActionParameters' {} a -> s {global = a} :: ActionParameters)

-- | The unique ID of the component that the @ActionParameters@ apply to.
actionParameters_id :: Lens.Lens' ActionParameters (Prelude.Maybe ComponentProperty)
actionParameters_id = Lens.lens (\ActionParameters' {id} -> id) (\s@ActionParameters' {} a -> s {id = a} :: ActionParameters)

-- | The name of the data model. Use when the action performs an operation on
-- an Amplify DataStore model.
actionParameters_model :: Lens.Lens' ActionParameters (Prelude.Maybe Prelude.Text)
actionParameters_model = Lens.lens (\ActionParameters' {model} -> model) (\s@ActionParameters' {} a -> s {model = a} :: ActionParameters)

-- | A key-value pair that specifies the state property name and its initial
-- value.
actionParameters_state :: Lens.Lens' ActionParameters (Prelude.Maybe MutationActionSetStateParameter)
actionParameters_state = Lens.lens (\ActionParameters' {state} -> state) (\s@ActionParameters' {} a -> s {state = a} :: ActionParameters)

-- | The element within the same component to modify when the action occurs.
actionParameters_target :: Lens.Lens' ActionParameters (Prelude.Maybe ComponentProperty)
actionParameters_target = Lens.lens (\ActionParameters' {target} -> target) (\s@ActionParameters' {} a -> s {target = a} :: ActionParameters)

-- | The type of navigation action. Valid values are @url@ and @anchor@. This
-- value is required for a navigation action.
actionParameters_type :: Lens.Lens' ActionParameters (Prelude.Maybe ComponentProperty)
actionParameters_type = Lens.lens (\ActionParameters' {type'} -> type') (\s@ActionParameters' {} a -> s {type' = a} :: ActionParameters)

-- | The URL to the location to open. Specify this value for a navigation
-- action.
actionParameters_url :: Lens.Lens' ActionParameters (Prelude.Maybe ComponentProperty)
actionParameters_url = Lens.lens (\ActionParameters' {url} -> url) (\s@ActionParameters' {} a -> s {url = a} :: ActionParameters)

instance Data.FromJSON ActionParameters where
  parseJSON =
    Data.withObject
      "ActionParameters"
      ( \x ->
          ActionParameters'
            Prelude.<$> (x Data..:? "anchor")
            Prelude.<*> (x Data..:? "fields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "global")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "model")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "target")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable ActionParameters where
  hashWithSalt _salt ActionParameters' {..} =
    _salt
      `Prelude.hashWithSalt` anchor
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` global
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` url

instance Prelude.NFData ActionParameters where
  rnf ActionParameters' {..} =
    Prelude.rnf anchor `Prelude.seq`
      Prelude.rnf fields `Prelude.seq`
        Prelude.rnf global `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf model `Prelude.seq`
              Prelude.rnf state `Prelude.seq`
                Prelude.rnf target `Prelude.seq`
                  Prelude.rnf type' `Prelude.seq`
                    Prelude.rnf url

instance Data.ToJSON ActionParameters where
  toJSON ActionParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("anchor" Data..=) Prelude.<$> anchor,
            ("fields" Data..=) Prelude.<$> fields,
            ("global" Data..=) Prelude.<$> global,
            ("id" Data..=) Prelude.<$> id,
            ("model" Data..=) Prelude.<$> model,
            ("state" Data..=) Prelude.<$> state,
            ("target" Data..=) Prelude.<$> target,
            ("type" Data..=) Prelude.<$> type',
            ("url" Data..=) Prelude.<$> url
          ]
      )
