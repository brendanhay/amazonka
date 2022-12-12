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
  { -- | The action to perform when updating the attribute.
    action :: Prelude.Maybe UpdateActionType,
    -- | The attribute to update.
    attribute :: Prelude.Maybe FacetAttribute
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
-- 'action', 'facetAttributeUpdate_action' - The action to perform when updating the attribute.
--
-- 'attribute', 'facetAttributeUpdate_attribute' - The attribute to update.
newFacetAttributeUpdate ::
  FacetAttributeUpdate
newFacetAttributeUpdate =
  FacetAttributeUpdate'
    { action = Prelude.Nothing,
      attribute = Prelude.Nothing
    }

-- | The action to perform when updating the attribute.
facetAttributeUpdate_action :: Lens.Lens' FacetAttributeUpdate (Prelude.Maybe UpdateActionType)
facetAttributeUpdate_action = Lens.lens (\FacetAttributeUpdate' {action} -> action) (\s@FacetAttributeUpdate' {} a -> s {action = a} :: FacetAttributeUpdate)

-- | The attribute to update.
facetAttributeUpdate_attribute :: Lens.Lens' FacetAttributeUpdate (Prelude.Maybe FacetAttribute)
facetAttributeUpdate_attribute = Lens.lens (\FacetAttributeUpdate' {attribute} -> attribute) (\s@FacetAttributeUpdate' {} a -> s {attribute = a} :: FacetAttributeUpdate)

instance Prelude.Hashable FacetAttributeUpdate where
  hashWithSalt _salt FacetAttributeUpdate' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` attribute

instance Prelude.NFData FacetAttributeUpdate where
  rnf FacetAttributeUpdate' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf attribute

instance Data.ToJSON FacetAttributeUpdate where
  toJSON FacetAttributeUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Action" Data..=) Prelude.<$> action,
            ("Attribute" Data..=) Prelude.<$> attribute
          ]
      )
