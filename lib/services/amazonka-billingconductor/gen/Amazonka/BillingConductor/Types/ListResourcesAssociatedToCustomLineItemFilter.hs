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
-- Module      : Amazonka.BillingConductor.Types.ListResourcesAssociatedToCustomLineItemFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListResourcesAssociatedToCustomLineItemFilter where

import Amazonka.BillingConductor.Types.CustomLineItemRelationship
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter that specifies the type of resource associations that should be
-- retrieved for a custom line item.
--
-- /See:/ 'newListResourcesAssociatedToCustomLineItemFilter' smart constructor.
data ListResourcesAssociatedToCustomLineItemFilter = ListResourcesAssociatedToCustomLineItemFilter'
  { -- | The type of relationship between the custom line item and the associated
    -- resource.
    relationship :: Prelude.Maybe CustomLineItemRelationship
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesAssociatedToCustomLineItemFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationship', 'listResourcesAssociatedToCustomLineItemFilter_relationship' - The type of relationship between the custom line item and the associated
-- resource.
newListResourcesAssociatedToCustomLineItemFilter ::
  ListResourcesAssociatedToCustomLineItemFilter
newListResourcesAssociatedToCustomLineItemFilter =
  ListResourcesAssociatedToCustomLineItemFilter'
    { relationship =
        Prelude.Nothing
    }

-- | The type of relationship between the custom line item and the associated
-- resource.
listResourcesAssociatedToCustomLineItemFilter_relationship :: Lens.Lens' ListResourcesAssociatedToCustomLineItemFilter (Prelude.Maybe CustomLineItemRelationship)
listResourcesAssociatedToCustomLineItemFilter_relationship = Lens.lens (\ListResourcesAssociatedToCustomLineItemFilter' {relationship} -> relationship) (\s@ListResourcesAssociatedToCustomLineItemFilter' {} a -> s {relationship = a} :: ListResourcesAssociatedToCustomLineItemFilter)

instance
  Prelude.Hashable
    ListResourcesAssociatedToCustomLineItemFilter
  where
  hashWithSalt
    _salt
    ListResourcesAssociatedToCustomLineItemFilter' {..} =
      _salt `Prelude.hashWithSalt` relationship

instance
  Prelude.NFData
    ListResourcesAssociatedToCustomLineItemFilter
  where
  rnf
    ListResourcesAssociatedToCustomLineItemFilter' {..} =
      Prelude.rnf relationship

instance
  Data.ToJSON
    ListResourcesAssociatedToCustomLineItemFilter
  where
  toJSON
    ListResourcesAssociatedToCustomLineItemFilter' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Relationship" Data..=) Prelude.<$> relationship]
        )
