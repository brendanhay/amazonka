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
-- Module      : Amazonka.CloudFront.Types.StagingDistributionDnsNames
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.StagingDistributionDnsNames where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The CloudFront domain name of the staging distribution.
--
-- /See:/ 'newStagingDistributionDnsNames' smart constructor.
data StagingDistributionDnsNames = StagingDistributionDnsNames'
  { -- | The CloudFront domain name of the staging distribution.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of CloudFront domain names in your staging distribution.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StagingDistributionDnsNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'stagingDistributionDnsNames_items' - The CloudFront domain name of the staging distribution.
--
-- 'quantity', 'stagingDistributionDnsNames_quantity' - The number of CloudFront domain names in your staging distribution.
newStagingDistributionDnsNames ::
  -- | 'quantity'
  Prelude.Int ->
  StagingDistributionDnsNames
newStagingDistributionDnsNames pQuantity_ =
  StagingDistributionDnsNames'
    { items =
        Prelude.Nothing,
      quantity = pQuantity_
    }

-- | The CloudFront domain name of the staging distribution.
stagingDistributionDnsNames_items :: Lens.Lens' StagingDistributionDnsNames (Prelude.Maybe [Prelude.Text])
stagingDistributionDnsNames_items = Lens.lens (\StagingDistributionDnsNames' {items} -> items) (\s@StagingDistributionDnsNames' {} a -> s {items = a} :: StagingDistributionDnsNames) Prelude.. Lens.mapping Lens.coerced

-- | The number of CloudFront domain names in your staging distribution.
stagingDistributionDnsNames_quantity :: Lens.Lens' StagingDistributionDnsNames Prelude.Int
stagingDistributionDnsNames_quantity = Lens.lens (\StagingDistributionDnsNames' {quantity} -> quantity) (\s@StagingDistributionDnsNames' {} a -> s {quantity = a} :: StagingDistributionDnsNames)

instance Core.FromXML StagingDistributionDnsNames where
  parseXML x =
    StagingDistributionDnsNames'
      Prelude.<$> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "DnsName")
                  )
      Prelude.<*> (x Core..@ "Quantity")

instance Prelude.Hashable StagingDistributionDnsNames where
  hashWithSalt _salt StagingDistributionDnsNames' {..} =
    _salt `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData StagingDistributionDnsNames where
  rnf StagingDistributionDnsNames' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance Core.ToXML StagingDistributionDnsNames where
  toXML StagingDistributionDnsNames' {..} =
    Prelude.mconcat
      [ "Items"
          Core.@= Core.toXML
            (Core.toXMLList "DnsName" Prelude.<$> items),
        "Quantity" Core.@= quantity
      ]
