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
-- Module      : Amazonka.CloudDirectory.Types.FacetAttributeUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.FacetAttributeUpdate where

import Amazonka.CloudDirectory.Types.FacetAttribute
import Amazonka.CloudDirectory.Types.UpdateActionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information used to update an attribute.
--
-- /See:/ 'newFacetAttributeUpdate' smart constructor.
data FacetAttributeUpdate = FacetAttributeUpdate'
  { -- | The attribute to update.
    attribute :: Prelude.Maybe FacetAttribute,
    -- | The action to perform when updating the attribute.
    action :: Prelude.Maybe UpdateActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FacetAttributeUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'facetAttributeUpdate_attribute' - The attribute to update.
--
-- 'action', 'facetAttributeUpdate_action' - The action to perform when updating the attribute.
newFacetAttributeUpdate ::
  FacetAttributeUpdate
newFacetAttributeUpdate =
  FacetAttributeUpdate'
    { attribute = Prelude.Nothing,
      action = Prelude.Nothing
    }

-- | The attribute to update.
facetAttributeUpdate_attribute :: Lens.Lens' FacetAttributeUpdate (Prelude.Maybe FacetAttribute)
facetAttributeUpdate_attribute = Lens.lens (\FacetAttributeUpdate' {attribute} -> attribute) (\s@FacetAttributeUpdate' {} a -> s {attribute = a} :: FacetAttributeUpdate)

-- | The action to perform when updating the attribute.
facetAttributeUpdate_action :: Lens.Lens' FacetAttributeUpdate (Prelude.Maybe UpdateActionType)
facetAttributeUpdate_action = Lens.lens (\FacetAttributeUpdate' {action} -> action) (\s@FacetAttributeUpdate' {} a -> s {action = a} :: FacetAttributeUpdate)

instance Prelude.Hashable FacetAttributeUpdate where
  hashWithSalt _salt FacetAttributeUpdate' {..} =
    _salt `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` action

instance Prelude.NFData FacetAttributeUpdate where
  rnf FacetAttributeUpdate' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf action

instance Data.ToJSON FacetAttributeUpdate where
  toJSON FacetAttributeUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attribute" Data..=) Prelude.<$> attribute,
            ("Action" Data..=) Prelude.<$> action
          ]
      )
