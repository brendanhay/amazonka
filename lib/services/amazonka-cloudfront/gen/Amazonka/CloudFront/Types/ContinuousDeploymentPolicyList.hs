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
-- Module      : Amazonka.CloudFront.Types.ContinuousDeploymentPolicyList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.ContinuousDeploymentPolicyList where

import Amazonka.CloudFront.Types.ContinuousDeploymentPolicySummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a list of continuous deployment policies.
--
-- /See:/ 'newContinuousDeploymentPolicyList' smart constructor.
data ContinuousDeploymentPolicyList = ContinuousDeploymentPolicyList'
  { -- | A list of continuous deployment policy items.
    items :: Prelude.Maybe [ContinuousDeploymentPolicySummary],
    -- | Indicates the next page of continuous deployment policies. To get the
    -- next page of the list, use this value in the @Marker@ field of your
    -- request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of continuous deployment policies that were specified
    -- in your request.
    maxItems :: Prelude.Int,
    -- | The total number of continuous deployment policies in your Amazon Web
    -- Services account, regardless of the @MaxItems@ value.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousDeploymentPolicyList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'continuousDeploymentPolicyList_items' - A list of continuous deployment policy items.
--
-- 'nextMarker', 'continuousDeploymentPolicyList_nextMarker' - Indicates the next page of continuous deployment policies. To get the
-- next page of the list, use this value in the @Marker@ field of your
-- request.
--
-- 'maxItems', 'continuousDeploymentPolicyList_maxItems' - The maximum number of continuous deployment policies that were specified
-- in your request.
--
-- 'quantity', 'continuousDeploymentPolicyList_quantity' - The total number of continuous deployment policies in your Amazon Web
-- Services account, regardless of the @MaxItems@ value.
newContinuousDeploymentPolicyList ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'quantity'
  Prelude.Int ->
  ContinuousDeploymentPolicyList
newContinuousDeploymentPolicyList
  pMaxItems_
  pQuantity_ =
    ContinuousDeploymentPolicyList'
      { items =
          Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        maxItems = pMaxItems_,
        quantity = pQuantity_
      }

-- | A list of continuous deployment policy items.
continuousDeploymentPolicyList_items :: Lens.Lens' ContinuousDeploymentPolicyList (Prelude.Maybe [ContinuousDeploymentPolicySummary])
continuousDeploymentPolicyList_items = Lens.lens (\ContinuousDeploymentPolicyList' {items} -> items) (\s@ContinuousDeploymentPolicyList' {} a -> s {items = a} :: ContinuousDeploymentPolicyList) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the next page of continuous deployment policies. To get the
-- next page of the list, use this value in the @Marker@ field of your
-- request.
continuousDeploymentPolicyList_nextMarker :: Lens.Lens' ContinuousDeploymentPolicyList (Prelude.Maybe Prelude.Text)
continuousDeploymentPolicyList_nextMarker = Lens.lens (\ContinuousDeploymentPolicyList' {nextMarker} -> nextMarker) (\s@ContinuousDeploymentPolicyList' {} a -> s {nextMarker = a} :: ContinuousDeploymentPolicyList)

-- | The maximum number of continuous deployment policies that were specified
-- in your request.
continuousDeploymentPolicyList_maxItems :: Lens.Lens' ContinuousDeploymentPolicyList Prelude.Int
continuousDeploymentPolicyList_maxItems = Lens.lens (\ContinuousDeploymentPolicyList' {maxItems} -> maxItems) (\s@ContinuousDeploymentPolicyList' {} a -> s {maxItems = a} :: ContinuousDeploymentPolicyList)

-- | The total number of continuous deployment policies in your Amazon Web
-- Services account, regardless of the @MaxItems@ value.
continuousDeploymentPolicyList_quantity :: Lens.Lens' ContinuousDeploymentPolicyList Prelude.Int
continuousDeploymentPolicyList_quantity = Lens.lens (\ContinuousDeploymentPolicyList' {quantity} -> quantity) (\s@ContinuousDeploymentPolicyList' {} a -> s {quantity = a} :: ContinuousDeploymentPolicyList)

instance Data.FromXML ContinuousDeploymentPolicyList where
  parseXML x =
    ContinuousDeploymentPolicyList'
      Prelude.<$> ( x Data..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Data.parseXMLList
                            "ContinuousDeploymentPolicySummary"
                        )
                  )
      Prelude.<*> (x Data..@? "NextMarker")
      Prelude.<*> (x Data..@ "MaxItems")
      Prelude.<*> (x Data..@ "Quantity")

instance
  Prelude.Hashable
    ContinuousDeploymentPolicyList
  where
  hashWithSalt
    _salt
    ContinuousDeploymentPolicyList' {..} =
      _salt `Prelude.hashWithSalt` items
        `Prelude.hashWithSalt` nextMarker
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` quantity

instance
  Prelude.NFData
    ContinuousDeploymentPolicyList
  where
  rnf ContinuousDeploymentPolicyList' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf quantity
