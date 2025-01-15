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
-- Module      : Amazonka.AmplifyUiBuilder.Types.MutationActionSetStateParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.MutationActionSetStateParameter where

import Amazonka.AmplifyUiBuilder.Types.ComponentProperty
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the state configuration when an action modifies a property of
-- another element within the same component.
--
-- /See:/ 'newMutationActionSetStateParameter' smart constructor.
data MutationActionSetStateParameter = MutationActionSetStateParameter'
  { -- | The name of the component that is being modified.
    componentName :: Prelude.Text,
    -- | The name of the component property to apply the state configuration to.
    property :: Prelude.Text,
    -- | The state configuration to assign to the property.
    set :: ComponentProperty
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MutationActionSetStateParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentName', 'mutationActionSetStateParameter_componentName' - The name of the component that is being modified.
--
-- 'property', 'mutationActionSetStateParameter_property' - The name of the component property to apply the state configuration to.
--
-- 'set', 'mutationActionSetStateParameter_set' - The state configuration to assign to the property.
newMutationActionSetStateParameter ::
  -- | 'componentName'
  Prelude.Text ->
  -- | 'property'
  Prelude.Text ->
  -- | 'set'
  ComponentProperty ->
  MutationActionSetStateParameter
newMutationActionSetStateParameter
  pComponentName_
  pProperty_
  pSet_ =
    MutationActionSetStateParameter'
      { componentName =
          pComponentName_,
        property = pProperty_,
        set = pSet_
      }

-- | The name of the component that is being modified.
mutationActionSetStateParameter_componentName :: Lens.Lens' MutationActionSetStateParameter Prelude.Text
mutationActionSetStateParameter_componentName = Lens.lens (\MutationActionSetStateParameter' {componentName} -> componentName) (\s@MutationActionSetStateParameter' {} a -> s {componentName = a} :: MutationActionSetStateParameter)

-- | The name of the component property to apply the state configuration to.
mutationActionSetStateParameter_property :: Lens.Lens' MutationActionSetStateParameter Prelude.Text
mutationActionSetStateParameter_property = Lens.lens (\MutationActionSetStateParameter' {property} -> property) (\s@MutationActionSetStateParameter' {} a -> s {property = a} :: MutationActionSetStateParameter)

-- | The state configuration to assign to the property.
mutationActionSetStateParameter_set :: Lens.Lens' MutationActionSetStateParameter ComponentProperty
mutationActionSetStateParameter_set = Lens.lens (\MutationActionSetStateParameter' {set} -> set) (\s@MutationActionSetStateParameter' {} a -> s {set = a} :: MutationActionSetStateParameter)

instance
  Data.FromJSON
    MutationActionSetStateParameter
  where
  parseJSON =
    Data.withObject
      "MutationActionSetStateParameter"
      ( \x ->
          MutationActionSetStateParameter'
            Prelude.<$> (x Data..: "componentName")
            Prelude.<*> (x Data..: "property")
            Prelude.<*> (x Data..: "set")
      )

instance
  Prelude.Hashable
    MutationActionSetStateParameter
  where
  hashWithSalt
    _salt
    MutationActionSetStateParameter' {..} =
      _salt
        `Prelude.hashWithSalt` componentName
        `Prelude.hashWithSalt` property
        `Prelude.hashWithSalt` set

instance
  Prelude.NFData
    MutationActionSetStateParameter
  where
  rnf MutationActionSetStateParameter' {..} =
    Prelude.rnf componentName `Prelude.seq`
      Prelude.rnf property `Prelude.seq`
        Prelude.rnf set

instance Data.ToJSON MutationActionSetStateParameter where
  toJSON MutationActionSetStateParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("componentName" Data..= componentName),
            Prelude.Just ("property" Data..= property),
            Prelude.Just ("set" Data..= set)
          ]
      )
