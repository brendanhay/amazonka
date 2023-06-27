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
-- Module      : Amazonka.CloudFront.Types.Distribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.Distribution where

import Amazonka.CloudFront.Types.ActiveTrustedKeyGroups
import Amazonka.CloudFront.Types.ActiveTrustedSigners
import Amazonka.CloudFront.Types.AliasICPRecordal
import Amazonka.CloudFront.Types.DistributionConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A distribution tells CloudFront where you want content to be delivered
-- from, and the details about how to track and manage content delivery.
--
-- /See:/ 'newDistribution' smart constructor.
data Distribution = Distribution'
  { -- | This field contains a list of key groups and the public keys in each key
    -- group that CloudFront can use to verify the signatures of signed URLs or
    -- signed cookies.
    activeTrustedKeyGroups :: Prelude.Maybe ActiveTrustedKeyGroups,
    -- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
    --
    -- This field contains a list of Amazon Web Services account IDs and the
    -- active CloudFront key pairs in each account that CloudFront can use to
    -- verify the signatures of signed URLs or signed cookies.
    activeTrustedSigners :: Prelude.Maybe ActiveTrustedSigners,
    -- | Amazon Web Services services in China customers must file for an
    -- Internet Content Provider (ICP) recordal if they want to serve content
    -- publicly on an alternate domain name, also known as a CNAME, that
    -- they\'ve added to CloudFront. AliasICPRecordal provides the ICP recordal
    -- status for CNAMEs associated with distributions.
    --
    -- For more information about ICP recordals, see
    -- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
    -- in /Getting Started with Amazon Web Services services in China/.
    aliasICPRecordals :: Prelude.Maybe [AliasICPRecordal],
    -- | The distribution\'s identifier. For example: @E1U5RQF7T870K0@.
    id :: Prelude.Text,
    -- | The distribution\'s Amazon Resource Name (ARN).
    arn :: Prelude.Text,
    -- | The distribution\'s status. When the status is @Deployed@, the
    -- distribution\'s information is fully propagated to all CloudFront edge
    -- locations.
    status :: Prelude.Text,
    -- | The date and time when the distribution was last modified.
    lastModifiedTime :: Data.ISO8601,
    -- | The number of invalidation batches currently in progress.
    inProgressInvalidationBatches :: Prelude.Int,
    -- | The distribution\'s CloudFront domain name. For example:
    -- @d111111abcdef8.cloudfront.net@.
    domainName :: Prelude.Text,
    -- | The distribution\'s configuration.
    distributionConfig :: DistributionConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Distribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeTrustedKeyGroups', 'distribution_activeTrustedKeyGroups' - This field contains a list of key groups and the public keys in each key
-- group that CloudFront can use to verify the signatures of signed URLs or
-- signed cookies.
--
-- 'activeTrustedSigners', 'distribution_activeTrustedSigners' - We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- This field contains a list of Amazon Web Services account IDs and the
-- active CloudFront key pairs in each account that CloudFront can use to
-- verify the signatures of signed URLs or signed cookies.
--
-- 'aliasICPRecordals', 'distribution_aliasICPRecordals' - Amazon Web Services services in China customers must file for an
-- Internet Content Provider (ICP) recordal if they want to serve content
-- publicly on an alternate domain name, also known as a CNAME, that
-- they\'ve added to CloudFront. AliasICPRecordal provides the ICP recordal
-- status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see
-- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
-- in /Getting Started with Amazon Web Services services in China/.
--
-- 'id', 'distribution_id' - The distribution\'s identifier. For example: @E1U5RQF7T870K0@.
--
-- 'arn', 'distribution_arn' - The distribution\'s Amazon Resource Name (ARN).
--
-- 'status', 'distribution_status' - The distribution\'s status. When the status is @Deployed@, the
-- distribution\'s information is fully propagated to all CloudFront edge
-- locations.
--
-- 'lastModifiedTime', 'distribution_lastModifiedTime' - The date and time when the distribution was last modified.
--
-- 'inProgressInvalidationBatches', 'distribution_inProgressInvalidationBatches' - The number of invalidation batches currently in progress.
--
-- 'domainName', 'distribution_domainName' - The distribution\'s CloudFront domain name. For example:
-- @d111111abcdef8.cloudfront.net@.
--
-- 'distributionConfig', 'distribution_distributionConfig' - The distribution\'s configuration.
newDistribution ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'inProgressInvalidationBatches'
  Prelude.Int ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'distributionConfig'
  DistributionConfig ->
  Distribution
newDistribution
  pId_
  pARN_
  pStatus_
  pLastModifiedTime_
  pInProgressInvalidationBatches_
  pDomainName_
  pDistributionConfig_ =
    Distribution'
      { activeTrustedKeyGroups =
          Prelude.Nothing,
        activeTrustedSigners = Prelude.Nothing,
        aliasICPRecordals = Prelude.Nothing,
        id = pId_,
        arn = pARN_,
        status = pStatus_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        inProgressInvalidationBatches =
          pInProgressInvalidationBatches_,
        domainName = pDomainName_,
        distributionConfig = pDistributionConfig_
      }

-- | This field contains a list of key groups and the public keys in each key
-- group that CloudFront can use to verify the signatures of signed URLs or
-- signed cookies.
distribution_activeTrustedKeyGroups :: Lens.Lens' Distribution (Prelude.Maybe ActiveTrustedKeyGroups)
distribution_activeTrustedKeyGroups = Lens.lens (\Distribution' {activeTrustedKeyGroups} -> activeTrustedKeyGroups) (\s@Distribution' {} a -> s {activeTrustedKeyGroups = a} :: Distribution)

-- | We recommend using @TrustedKeyGroups@ instead of @TrustedSigners@.
--
-- This field contains a list of Amazon Web Services account IDs and the
-- active CloudFront key pairs in each account that CloudFront can use to
-- verify the signatures of signed URLs or signed cookies.
distribution_activeTrustedSigners :: Lens.Lens' Distribution (Prelude.Maybe ActiveTrustedSigners)
distribution_activeTrustedSigners = Lens.lens (\Distribution' {activeTrustedSigners} -> activeTrustedSigners) (\s@Distribution' {} a -> s {activeTrustedSigners = a} :: Distribution)

-- | Amazon Web Services services in China customers must file for an
-- Internet Content Provider (ICP) recordal if they want to serve content
-- publicly on an alternate domain name, also known as a CNAME, that
-- they\'ve added to CloudFront. AliasICPRecordal provides the ICP recordal
-- status for CNAMEs associated with distributions.
--
-- For more information about ICP recordals, see
-- <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials>
-- in /Getting Started with Amazon Web Services services in China/.
distribution_aliasICPRecordals :: Lens.Lens' Distribution (Prelude.Maybe [AliasICPRecordal])
distribution_aliasICPRecordals = Lens.lens (\Distribution' {aliasICPRecordals} -> aliasICPRecordals) (\s@Distribution' {} a -> s {aliasICPRecordals = a} :: Distribution) Prelude.. Lens.mapping Lens.coerced

-- | The distribution\'s identifier. For example: @E1U5RQF7T870K0@.
distribution_id :: Lens.Lens' Distribution Prelude.Text
distribution_id = Lens.lens (\Distribution' {id} -> id) (\s@Distribution' {} a -> s {id = a} :: Distribution)

-- | The distribution\'s Amazon Resource Name (ARN).
distribution_arn :: Lens.Lens' Distribution Prelude.Text
distribution_arn = Lens.lens (\Distribution' {arn} -> arn) (\s@Distribution' {} a -> s {arn = a} :: Distribution)

-- | The distribution\'s status. When the status is @Deployed@, the
-- distribution\'s information is fully propagated to all CloudFront edge
-- locations.
distribution_status :: Lens.Lens' Distribution Prelude.Text
distribution_status = Lens.lens (\Distribution' {status} -> status) (\s@Distribution' {} a -> s {status = a} :: Distribution)

-- | The date and time when the distribution was last modified.
distribution_lastModifiedTime :: Lens.Lens' Distribution Prelude.UTCTime
distribution_lastModifiedTime = Lens.lens (\Distribution' {lastModifiedTime} -> lastModifiedTime) (\s@Distribution' {} a -> s {lastModifiedTime = a} :: Distribution) Prelude.. Data._Time

-- | The number of invalidation batches currently in progress.
distribution_inProgressInvalidationBatches :: Lens.Lens' Distribution Prelude.Int
distribution_inProgressInvalidationBatches = Lens.lens (\Distribution' {inProgressInvalidationBatches} -> inProgressInvalidationBatches) (\s@Distribution' {} a -> s {inProgressInvalidationBatches = a} :: Distribution)

-- | The distribution\'s CloudFront domain name. For example:
-- @d111111abcdef8.cloudfront.net@.
distribution_domainName :: Lens.Lens' Distribution Prelude.Text
distribution_domainName = Lens.lens (\Distribution' {domainName} -> domainName) (\s@Distribution' {} a -> s {domainName = a} :: Distribution)

-- | The distribution\'s configuration.
distribution_distributionConfig :: Lens.Lens' Distribution DistributionConfig
distribution_distributionConfig = Lens.lens (\Distribution' {distributionConfig} -> distributionConfig) (\s@Distribution' {} a -> s {distributionConfig = a} :: Distribution)

instance Data.FromXML Distribution where
  parseXML x =
    Distribution'
      Prelude.<$> (x Data..@? "ActiveTrustedKeyGroups")
      Prelude.<*> (x Data..@? "ActiveTrustedSigners")
      Prelude.<*> ( x
                      Data..@? "AliasICPRecordals"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "AliasICPRecordal")
                  )
      Prelude.<*> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "ARN")
      Prelude.<*> (x Data..@ "Status")
      Prelude.<*> (x Data..@ "LastModifiedTime")
      Prelude.<*> (x Data..@ "InProgressInvalidationBatches")
      Prelude.<*> (x Data..@ "DomainName")
      Prelude.<*> (x Data..@ "DistributionConfig")

instance Prelude.Hashable Distribution where
  hashWithSalt _salt Distribution' {..} =
    _salt
      `Prelude.hashWithSalt` activeTrustedKeyGroups
      `Prelude.hashWithSalt` activeTrustedSigners
      `Prelude.hashWithSalt` aliasICPRecordals
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` inProgressInvalidationBatches
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` distributionConfig

instance Prelude.NFData Distribution where
  rnf Distribution' {..} =
    Prelude.rnf activeTrustedKeyGroups
      `Prelude.seq` Prelude.rnf activeTrustedSigners
      `Prelude.seq` Prelude.rnf aliasICPRecordals
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf inProgressInvalidationBatches
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf distributionConfig
