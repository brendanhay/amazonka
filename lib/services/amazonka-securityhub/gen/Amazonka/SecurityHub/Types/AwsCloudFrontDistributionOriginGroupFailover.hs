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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailover
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailover where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginGroupFailoverStatusCodes

-- | Provides information about when an origin group fails over.
--
-- /See:/ 'newAwsCloudFrontDistributionOriginGroupFailover' smart constructor.
data AwsCloudFrontDistributionOriginGroupFailover = AwsCloudFrontDistributionOriginGroupFailover'
  { -- | Information about the status codes that cause an origin group to fail
    -- over.
    statusCodes :: Prelude.Maybe AwsCloudFrontDistributionOriginGroupFailoverStatusCodes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionOriginGroupFailover' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusCodes', 'awsCloudFrontDistributionOriginGroupFailover_statusCodes' - Information about the status codes that cause an origin group to fail
-- over.
newAwsCloudFrontDistributionOriginGroupFailover ::
  AwsCloudFrontDistributionOriginGroupFailover
newAwsCloudFrontDistributionOriginGroupFailover =
  AwsCloudFrontDistributionOriginGroupFailover'
    { statusCodes =
        Prelude.Nothing
    }

-- | Information about the status codes that cause an origin group to fail
-- over.
awsCloudFrontDistributionOriginGroupFailover_statusCodes :: Lens.Lens' AwsCloudFrontDistributionOriginGroupFailover (Prelude.Maybe AwsCloudFrontDistributionOriginGroupFailoverStatusCodes)
awsCloudFrontDistributionOriginGroupFailover_statusCodes = Lens.lens (\AwsCloudFrontDistributionOriginGroupFailover' {statusCodes} -> statusCodes) (\s@AwsCloudFrontDistributionOriginGroupFailover' {} a -> s {statusCodes = a} :: AwsCloudFrontDistributionOriginGroupFailover)

instance
  Data.FromJSON
    AwsCloudFrontDistributionOriginGroupFailover
  where
  parseJSON =
    Data.withObject
      "AwsCloudFrontDistributionOriginGroupFailover"
      ( \x ->
          AwsCloudFrontDistributionOriginGroupFailover'
            Prelude.<$> (x Data..:? "StatusCodes")
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionOriginGroupFailover
  where
  hashWithSalt
    _salt
    AwsCloudFrontDistributionOriginGroupFailover' {..} =
      _salt `Prelude.hashWithSalt` statusCodes

instance
  Prelude.NFData
    AwsCloudFrontDistributionOriginGroupFailover
  where
  rnf AwsCloudFrontDistributionOriginGroupFailover' {..} =
    Prelude.rnf statusCodes

instance
  Data.ToJSON
    AwsCloudFrontDistributionOriginGroupFailover
  where
  toJSON
    AwsCloudFrontDistributionOriginGroupFailover' {..} =
      Data.object
        ( Prelude.catMaybes
            [("StatusCodes" Data..=) Prelude.<$> statusCodes]
        )
