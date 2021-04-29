{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeUpdate where

import Network.AWS.CloudDirectory.Types.FacetAttribute
import Network.AWS.CloudDirectory.Types.UpdateActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure that contains information used to update an attribute.
--
-- /See:/ 'newFacetAttributeUpdate' smart constructor.
data FacetAttributeUpdate = FacetAttributeUpdate'
  { -- | The attribute to update.
    attribute :: Prelude.Maybe FacetAttribute,
    -- | The action to perform when updating the attribute.
    action :: Prelude.Maybe UpdateActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable FacetAttributeUpdate

instance Prelude.NFData FacetAttributeUpdate

instance Prelude.ToJSON FacetAttributeUpdate where
  toJSON FacetAttributeUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Attribute" Prelude..=) Prelude.<$> attribute,
            ("Action" Prelude..=) Prelude.<$> action
          ]
      )
