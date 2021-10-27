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
-- Module      : Network.AWS.SecurityHub.Types.AwsCloudFrontDistributionOriginGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsCloudFrontDistributionOriginGroups where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsCloudFrontDistributionOriginGroup

-- | Provides information about origin groups that are associated with the
-- distribution.
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
  Core.FromJSON
    AwsCloudFrontDistributionOriginGroups
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionOriginGroups"
      ( \x ->
          AwsCloudFrontDistributionOriginGroups'
            Prelude.<$> (x Core..:? "Items" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionOriginGroups

instance
  Prelude.NFData
    AwsCloudFrontDistributionOriginGroups

instance
  Core.ToJSON
    AwsCloudFrontDistributionOriginGroups
  where
  toJSON AwsCloudFrontDistributionOriginGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Items" Core..=) Prelude.<$> items]
      )
