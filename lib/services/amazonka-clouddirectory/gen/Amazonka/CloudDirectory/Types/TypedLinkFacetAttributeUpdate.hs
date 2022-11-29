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
-- Module      : Amazonka.CloudDirectory.Types.TypedLinkFacetAttributeUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.TypedLinkFacetAttributeUpdate where

import Amazonka.CloudDirectory.Types.TypedLinkAttributeDefinition
import Amazonka.CloudDirectory.Types.UpdateActionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A typed link facet attribute update.
--
-- /See:/ 'newTypedLinkFacetAttributeUpdate' smart constructor.
data TypedLinkFacetAttributeUpdate = TypedLinkFacetAttributeUpdate'
  { -- | The attribute to update.
    attribute :: TypedLinkAttributeDefinition,
    -- | The action to perform when updating the attribute.
    action :: UpdateActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypedLinkFacetAttributeUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'typedLinkFacetAttributeUpdate_attribute' - The attribute to update.
--
-- 'action', 'typedLinkFacetAttributeUpdate_action' - The action to perform when updating the attribute.
newTypedLinkFacetAttributeUpdate ::
  -- | 'attribute'
  TypedLinkAttributeDefinition ->
  -- | 'action'
  UpdateActionType ->
  TypedLinkFacetAttributeUpdate
newTypedLinkFacetAttributeUpdate pAttribute_ pAction_ =
  TypedLinkFacetAttributeUpdate'
    { attribute =
        pAttribute_,
      action = pAction_
    }

-- | The attribute to update.
typedLinkFacetAttributeUpdate_attribute :: Lens.Lens' TypedLinkFacetAttributeUpdate TypedLinkAttributeDefinition
typedLinkFacetAttributeUpdate_attribute = Lens.lens (\TypedLinkFacetAttributeUpdate' {attribute} -> attribute) (\s@TypedLinkFacetAttributeUpdate' {} a -> s {attribute = a} :: TypedLinkFacetAttributeUpdate)

-- | The action to perform when updating the attribute.
typedLinkFacetAttributeUpdate_action :: Lens.Lens' TypedLinkFacetAttributeUpdate UpdateActionType
typedLinkFacetAttributeUpdate_action = Lens.lens (\TypedLinkFacetAttributeUpdate' {action} -> action) (\s@TypedLinkFacetAttributeUpdate' {} a -> s {action = a} :: TypedLinkFacetAttributeUpdate)

instance
  Prelude.Hashable
    TypedLinkFacetAttributeUpdate
  where
  hashWithSalt _salt TypedLinkFacetAttributeUpdate' {..} =
    _salt `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` action

instance Prelude.NFData TypedLinkFacetAttributeUpdate where
  rnf TypedLinkFacetAttributeUpdate' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf action

instance Core.ToJSON TypedLinkFacetAttributeUpdate where
  toJSON TypedLinkFacetAttributeUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Attribute" Core..= attribute),
            Prelude.Just ("Action" Core..= action)
          ]
      )
