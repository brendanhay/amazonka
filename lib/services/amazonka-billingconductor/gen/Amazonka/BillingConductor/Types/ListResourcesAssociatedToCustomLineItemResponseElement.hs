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
-- Module      : Amazonka.BillingConductor.Types.ListResourcesAssociatedToCustomLineItemResponseElement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.ListResourcesAssociatedToCustomLineItemResponseElement where

import Amazonka.BillingConductor.Types.CustomLineItemRelationship
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of a resource association for a custom line item.
--
-- /See:/ 'newListResourcesAssociatedToCustomLineItemResponseElement' smart constructor.
data ListResourcesAssociatedToCustomLineItemResponseElement = ListResourcesAssociatedToCustomLineItemResponseElement'
  { -- | The ARN of the associated resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The end billing period of the associated resource.
    endBillingPeriod :: Prelude.Maybe Prelude.Text,
    -- | The type of relationship between the custom line item and the associated
    -- resource.
    relationship :: Prelude.Maybe CustomLineItemRelationship
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourcesAssociatedToCustomLineItemResponseElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listResourcesAssociatedToCustomLineItemResponseElement_arn' - The ARN of the associated resource.
--
-- 'endBillingPeriod', 'listResourcesAssociatedToCustomLineItemResponseElement_endBillingPeriod' - The end billing period of the associated resource.
--
-- 'relationship', 'listResourcesAssociatedToCustomLineItemResponseElement_relationship' - The type of relationship between the custom line item and the associated
-- resource.
newListResourcesAssociatedToCustomLineItemResponseElement ::
  ListResourcesAssociatedToCustomLineItemResponseElement
newListResourcesAssociatedToCustomLineItemResponseElement =
  ListResourcesAssociatedToCustomLineItemResponseElement'
    { arn =
        Prelude.Nothing,
      endBillingPeriod =
        Prelude.Nothing,
      relationship =
        Prelude.Nothing
    }

-- | The ARN of the associated resource.
listResourcesAssociatedToCustomLineItemResponseElement_arn :: Lens.Lens' ListResourcesAssociatedToCustomLineItemResponseElement (Prelude.Maybe Prelude.Text)
listResourcesAssociatedToCustomLineItemResponseElement_arn = Lens.lens (\ListResourcesAssociatedToCustomLineItemResponseElement' {arn} -> arn) (\s@ListResourcesAssociatedToCustomLineItemResponseElement' {} a -> s {arn = a} :: ListResourcesAssociatedToCustomLineItemResponseElement)

-- | The end billing period of the associated resource.
listResourcesAssociatedToCustomLineItemResponseElement_endBillingPeriod :: Lens.Lens' ListResourcesAssociatedToCustomLineItemResponseElement (Prelude.Maybe Prelude.Text)
listResourcesAssociatedToCustomLineItemResponseElement_endBillingPeriod = Lens.lens (\ListResourcesAssociatedToCustomLineItemResponseElement' {endBillingPeriod} -> endBillingPeriod) (\s@ListResourcesAssociatedToCustomLineItemResponseElement' {} a -> s {endBillingPeriod = a} :: ListResourcesAssociatedToCustomLineItemResponseElement)

-- | The type of relationship between the custom line item and the associated
-- resource.
listResourcesAssociatedToCustomLineItemResponseElement_relationship :: Lens.Lens' ListResourcesAssociatedToCustomLineItemResponseElement (Prelude.Maybe CustomLineItemRelationship)
listResourcesAssociatedToCustomLineItemResponseElement_relationship = Lens.lens (\ListResourcesAssociatedToCustomLineItemResponseElement' {relationship} -> relationship) (\s@ListResourcesAssociatedToCustomLineItemResponseElement' {} a -> s {relationship = a} :: ListResourcesAssociatedToCustomLineItemResponseElement)

instance
  Data.FromJSON
    ListResourcesAssociatedToCustomLineItemResponseElement
  where
  parseJSON =
    Data.withObject
      "ListResourcesAssociatedToCustomLineItemResponseElement"
      ( \x ->
          ListResourcesAssociatedToCustomLineItemResponseElement'
            Prelude.<$> (x Data..:? "Arn")
              Prelude.<*> (x Data..:? "EndBillingPeriod")
              Prelude.<*> (x Data..:? "Relationship")
      )

instance
  Prelude.Hashable
    ListResourcesAssociatedToCustomLineItemResponseElement
  where
  hashWithSalt
    _salt
    ListResourcesAssociatedToCustomLineItemResponseElement' {..} =
      _salt `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` endBillingPeriod
        `Prelude.hashWithSalt` relationship

instance
  Prelude.NFData
    ListResourcesAssociatedToCustomLineItemResponseElement
  where
  rnf
    ListResourcesAssociatedToCustomLineItemResponseElement' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf endBillingPeriod
        `Prelude.seq` Prelude.rnf relationship
