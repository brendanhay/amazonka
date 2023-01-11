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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroups where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroup

-- | Provides information about origin groups that are associated with the
-- CloudFront distribution.
--
-- /See:/ 'newAwsCloudFrontDistributionOriginGroups' smart constructor.
data AwsCloudFrontDistributionOriginGroups = AwsCloudFrontDistributionOriginGroups'
  { -- | The list of origin groups.
    items :: Prelude.Maybe [AwsCloudFrontDistributionOriginGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionOriginGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'awsCloudFrontDistributionOriginGroups_items' - The list of origin groups.
newAwsCloudFrontDistributionOriginGroups ::
  AwsCloudFrontDistributionOriginGroups
newAwsCloudFrontDistributionOriginGroups =
  AwsCloudFrontDistributionOriginGroups'
    { items =
        Prelude.Nothing
    }

-- | The list of origin groups.
awsCloudFrontDistributionOriginGroups_items :: Lens.Lens' AwsCloudFrontDistributionOriginGroups (Prelude.Maybe [AwsCloudFrontDistributionOriginGroup])
awsCloudFrontDistributionOriginGroups_items = Lens.lens (\AwsCloudFrontDistributionOriginGroups' {items} -> items) (\s@AwsCloudFrontDistributionOriginGroups' {} a -> s {items = a} :: AwsCloudFrontDistributionOriginGroups) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsCloudFrontDistributionOriginGroups
  where
  parseJSON =
    Data.withObject
      "AwsCloudFrontDistributionOriginGroups"
      ( \x ->
          AwsCloudFrontDistributionOriginGroups'
            Prelude.<$> (x Data..:? "Items" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionOriginGroups
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionOriginGroups' {..} =
      _salt `Prelude.hashWithSalt` items

instance
  Prelude.NFData
    AwsCloudFrontDistributionOriginGroups
  where
  rnf AwsCloudFrontDistributionOriginGroups' {..} =
    Prelude.rnf items

instance
  Data.ToJSON
    AwsCloudFrontDistributionOriginGroups
  where
  toJSON AwsCloudFrontDistributionOriginGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Items" Data..=) Prelude.<$> items]
      )
