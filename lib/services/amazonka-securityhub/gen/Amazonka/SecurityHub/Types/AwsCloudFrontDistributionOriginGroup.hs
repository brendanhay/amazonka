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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailover

-- | Information about an origin group for the CloudFront distribution.
--
-- /See:/ 'newAwsCloudFrontDistributionOriginGroup' smart constructor.
data AwsCloudFrontDistributionOriginGroup = AwsCloudFrontDistributionOriginGroup'
  { -- | Provides the criteria for an origin group to fail over.
    failoverCriteria :: Prelude.Maybe AwsCloudFrontDistributionOriginGroupFailover
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionOriginGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failoverCriteria', 'awsCloudFrontDistributionOriginGroup_failoverCriteria' - Provides the criteria for an origin group to fail over.
newAwsCloudFrontDistributionOriginGroup ::
  AwsCloudFrontDistributionOriginGroup
newAwsCloudFrontDistributionOriginGroup =
  AwsCloudFrontDistributionOriginGroup'
    { failoverCriteria =
        Prelude.Nothing
    }

-- | Provides the criteria for an origin group to fail over.
awsCloudFrontDistributionOriginGroup_failoverCriteria :: Lens.Lens' AwsCloudFrontDistributionOriginGroup (Prelude.Maybe AwsCloudFrontDistributionOriginGroupFailover)
awsCloudFrontDistributionOriginGroup_failoverCriteria = Lens.lens (\AwsCloudFrontDistributionOriginGroup' {failoverCriteria} -> failoverCriteria) (\s@AwsCloudFrontDistributionOriginGroup' {} a -> s {failoverCriteria = a} :: AwsCloudFrontDistributionOriginGroup)

instance
  Data.FromJSON
    AwsCloudFrontDistributionOriginGroup
  where
  parseJSON =
    Data.withObject
      "AwsCloudFrontDistributionOriginGroup"
      ( \x ->
          AwsCloudFrontDistributionOriginGroup'
            Prelude.<$> (x Data..:? "FailoverCriteria")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionOriginGroup
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionOriginGroup' {..} =
      _salt `Prelude.hashWithSalt` failoverCriteria

instance
  Prelude.NFData
    AwsCloudFrontDistributionOriginGroup
  where
  rnf AwsCloudFrontDistributionOriginGroup' {..} =
    Prelude.rnf failoverCriteria

instance
  Data.ToJSON
    AwsCloudFrontDistributionOriginGroup
  where
  toJSON AwsCloudFrontDistributionOriginGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FailoverCriteria" Data..=)
              Prelude.<$> failoverCriteria
          ]
      )
