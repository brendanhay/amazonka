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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginSslProtocols
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginSslProtocols where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A complex type that contains information about the SSL\/TLS protocols
-- that CloudFront can use when establishing an HTTPS connection with your
-- origin.
--
-- /See:/ 'newAwsCloudFrontDistributionOriginSslProtocols' smart constructor.
data AwsCloudFrontDistributionOriginSslProtocols = AwsCloudFrontDistributionOriginSslProtocols'
  { -- | A list that contains allowed SSL\/TLS protocols for this distribution.
    items :: Prelude.Maybe [Prelude.Text],
    -- | The number of SSL\/TLS protocols that you want to allow CloudFront to
    -- use when establishing an HTTPS connection with this origin.
    quantity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionOriginSslProtocols' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'awsCloudFrontDistributionOriginSslProtocols_items' - A list that contains allowed SSL\/TLS protocols for this distribution.
--
-- 'quantity', 'awsCloudFrontDistributionOriginSslProtocols_quantity' - The number of SSL\/TLS protocols that you want to allow CloudFront to
-- use when establishing an HTTPS connection with this origin.
newAwsCloudFrontDistributionOriginSslProtocols ::
  AwsCloudFrontDistributionOriginSslProtocols
newAwsCloudFrontDistributionOriginSslProtocols =
  AwsCloudFrontDistributionOriginSslProtocols'
    { items =
        Prelude.Nothing,
      quantity = Prelude.Nothing
    }

-- | A list that contains allowed SSL\/TLS protocols for this distribution.
awsCloudFrontDistributionOriginSslProtocols_items :: Lens.Lens' AwsCloudFrontDistributionOriginSslProtocols (Prelude.Maybe [Prelude.Text])
awsCloudFrontDistributionOriginSslProtocols_items = Lens.lens (\AwsCloudFrontDistributionOriginSslProtocols' {items} -> items) (\s@AwsCloudFrontDistributionOriginSslProtocols' {} a -> s {items = a} :: AwsCloudFrontDistributionOriginSslProtocols) Prelude.. Lens.mapping Lens.coerced

-- | The number of SSL\/TLS protocols that you want to allow CloudFront to
-- use when establishing an HTTPS connection with this origin.
awsCloudFrontDistributionOriginSslProtocols_quantity :: Lens.Lens' AwsCloudFrontDistributionOriginSslProtocols (Prelude.Maybe Prelude.Int)
awsCloudFrontDistributionOriginSslProtocols_quantity = Lens.lens (\AwsCloudFrontDistributionOriginSslProtocols' {quantity} -> quantity) (\s@AwsCloudFrontDistributionOriginSslProtocols' {} a -> s {quantity = a} :: AwsCloudFrontDistributionOriginSslProtocols)

instance
  Data.FromJSON
    AwsCloudFrontDistributionOriginSslProtocols
  where
  parseJSON =
    Data.withObject
      "AwsCloudFrontDistributionOriginSslProtocols"
      ( \x ->
          AwsCloudFrontDistributionOriginSslProtocols'
            Prelude.<$> (x Data..:? "Items" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Quantity")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionOriginSslProtocols
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionOriginSslProtocols' {..} =
      _salt `Prelude.hashWithSalt` items
        `Prelude.hashWithSalt` quantity

instance
  Prelude.NFData
    AwsCloudFrontDistributionOriginSslProtocols
  where
  rnf AwsCloudFrontDistributionOriginSslProtocols' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity

instance
  Data.ToJSON
    AwsCloudFrontDistributionOriginSslProtocols
  where
  toJSON
    AwsCloudFrontDistributionOriginSslProtocols' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Items" Data..=) Prelude.<$> items,
              ("Quantity" Data..=) Prelude.<$> quantity
            ]
        )
