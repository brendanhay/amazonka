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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailoverStatusCodes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status codes that cause an origin group to fail over.
--
-- /See:/ 'newAwsCloudFrontDistributionOriginGroupFailoverStatusCodes' smart constructor.
data AwsCloudFrontDistributionOriginGroupFailoverStatusCodes = AwsCloudFrontDistributionOriginGroupFailoverStatusCodes'
  { -- | The list of status code values that can cause a failover to the next
    -- origin.
    items :: Prelude.Maybe [Prelude.Int],
    -- | The number of status codes that can cause a failover.
    quantity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionOriginGroupFailoverStatusCodes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'awsCloudFrontDistributionOriginGroupFailoverStatusCodes_items' - The list of status code values that can cause a failover to the next
-- origin.
--
-- 'quantity', 'awsCloudFrontDistributionOriginGroupFailoverStatusCodes_quantity' - The number of status codes that can cause a failover.
newAwsCloudFrontDistributionOriginGroupFailoverStatusCodes ::
  AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
newAwsCloudFrontDistributionOriginGroupFailoverStatusCodes =
  AwsCloudFrontDistributionOriginGroupFailoverStatusCodes'
    { items =
        Prelude.Nothing,
      quantity =
        Prelude.Nothing
    }

-- | The list of status code values that can cause a failover to the next
-- origin.
awsCloudFrontDistributionOriginGroupFailoverStatusCodes_items :: Lens.Lens' AwsCloudFrontDistributionOriginGroupFailoverStatusCodes (Prelude.Maybe [Prelude.Int])
awsCloudFrontDistributionOriginGroupFailoverStatusCodes_items = Lens.lens (\AwsCloudFrontDistributionOriginGroupFailoverStatusCodes' {items} -> items) (\s@AwsCloudFrontDistributionOriginGroupFailoverStatusCodes' {} a -> s {items = a} :: AwsCloudFrontDistributionOriginGroupFailoverStatusCodes) Prelude.. Lens.mapping Lens.coerced

-- | The number of status codes that can cause a failover.
awsCloudFrontDistributionOriginGroupFailoverStatusCodes_quantity :: Lens.Lens' AwsCloudFrontDistributionOriginGroupFailoverStatusCodes (Prelude.Maybe Prelude.Int)
awsCloudFrontDistributionOriginGroupFailoverStatusCodes_quantity = Lens.lens (\AwsCloudFrontDistributionOriginGroupFailoverStatusCodes' {quantity} -> quantity) (\s@AwsCloudFrontDistributionOriginGroupFailoverStatusCodes' {} a -> s {quantity = a} :: AwsCloudFrontDistributionOriginGroupFailoverStatusCodes)

instance
  Data.FromJSON
    AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
  where
  parseJSON =
    Data.withObject
      "AwsCloudFrontDistributionOriginGroupFailoverStatusCodes"
      ( \x ->
          AwsCloudFrontDistributionOriginGroupFailoverStatusCodes'
            Prelude.<$> (x Data..:? "Items" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Quantity")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionOriginGroupFailoverStatusCodes' {..} =
      _salt
        `Prelude.hashWithSalt` items
        `Prelude.hashWithSalt` quantity

instance
  Prelude.NFData
    AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
  where
  rnf
    AwsCloudFrontDistributionOriginGroupFailoverStatusCodes' {..} =
      Prelude.rnf items `Prelude.seq`
        Prelude.rnf quantity

instance
  Data.ToJSON
    AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
  where
  toJSON
    AwsCloudFrontDistributionOriginGroupFailoverStatusCodes' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Items" Data..=) Prelude.<$> items,
              ("Quantity" Data..=) Prelude.<$> quantity
            ]
        )
