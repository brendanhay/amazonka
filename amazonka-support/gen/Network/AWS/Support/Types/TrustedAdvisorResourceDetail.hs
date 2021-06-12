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
-- Module      : Network.AWS.Support.Types.TrustedAdvisorResourceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorResourceDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a resource identified by a Trusted Advisor
-- check.
--
-- /See:/ 'newTrustedAdvisorResourceDetail' smart constructor.
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail'
  { -- | Specifies whether the AWS resource was ignored by Trusted Advisor
    -- because it was marked as suppressed by the user.
    isSuppressed :: Core.Maybe Core.Bool,
    -- | The AWS region in which the identified resource is located.
    region :: Core.Maybe Core.Text,
    -- | The status code for the resource identified in the Trusted Advisor
    -- check.
    status :: Core.Text,
    -- | The unique identifier for the identified resource.
    resourceId :: Core.Text,
    -- | Additional information about the identified resource. The exact metadata
    -- and its order can be obtained by inspecting the
    -- TrustedAdvisorCheckDescription object returned by the call to
    -- DescribeTrustedAdvisorChecks. __Metadata__ contains all the data that is
    -- shown in the Excel download, even in those cases where the UI shows just
    -- summary data.
    metadata :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrustedAdvisorResourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isSuppressed', 'trustedAdvisorResourceDetail_isSuppressed' - Specifies whether the AWS resource was ignored by Trusted Advisor
-- because it was marked as suppressed by the user.
--
-- 'region', 'trustedAdvisorResourceDetail_region' - The AWS region in which the identified resource is located.
--
-- 'status', 'trustedAdvisorResourceDetail_status' - The status code for the resource identified in the Trusted Advisor
-- check.
--
-- 'resourceId', 'trustedAdvisorResourceDetail_resourceId' - The unique identifier for the identified resource.
--
-- 'metadata', 'trustedAdvisorResourceDetail_metadata' - Additional information about the identified resource. The exact metadata
-- and its order can be obtained by inspecting the
-- TrustedAdvisorCheckDescription object returned by the call to
-- DescribeTrustedAdvisorChecks. __Metadata__ contains all the data that is
-- shown in the Excel download, even in those cases where the UI shows just
-- summary data.
newTrustedAdvisorResourceDetail ::
  -- | 'status'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  TrustedAdvisorResourceDetail
newTrustedAdvisorResourceDetail pStatus_ pResourceId_ =
  TrustedAdvisorResourceDetail'
    { isSuppressed =
        Core.Nothing,
      region = Core.Nothing,
      status = pStatus_,
      resourceId = pResourceId_,
      metadata = Core.mempty
    }

-- | Specifies whether the AWS resource was ignored by Trusted Advisor
-- because it was marked as suppressed by the user.
trustedAdvisorResourceDetail_isSuppressed :: Lens.Lens' TrustedAdvisorResourceDetail (Core.Maybe Core.Bool)
trustedAdvisorResourceDetail_isSuppressed = Lens.lens (\TrustedAdvisorResourceDetail' {isSuppressed} -> isSuppressed) (\s@TrustedAdvisorResourceDetail' {} a -> s {isSuppressed = a} :: TrustedAdvisorResourceDetail)

-- | The AWS region in which the identified resource is located.
trustedAdvisorResourceDetail_region :: Lens.Lens' TrustedAdvisorResourceDetail (Core.Maybe Core.Text)
trustedAdvisorResourceDetail_region = Lens.lens (\TrustedAdvisorResourceDetail' {region} -> region) (\s@TrustedAdvisorResourceDetail' {} a -> s {region = a} :: TrustedAdvisorResourceDetail)

-- | The status code for the resource identified in the Trusted Advisor
-- check.
trustedAdvisorResourceDetail_status :: Lens.Lens' TrustedAdvisorResourceDetail Core.Text
trustedAdvisorResourceDetail_status = Lens.lens (\TrustedAdvisorResourceDetail' {status} -> status) (\s@TrustedAdvisorResourceDetail' {} a -> s {status = a} :: TrustedAdvisorResourceDetail)

-- | The unique identifier for the identified resource.
trustedAdvisorResourceDetail_resourceId :: Lens.Lens' TrustedAdvisorResourceDetail Core.Text
trustedAdvisorResourceDetail_resourceId = Lens.lens (\TrustedAdvisorResourceDetail' {resourceId} -> resourceId) (\s@TrustedAdvisorResourceDetail' {} a -> s {resourceId = a} :: TrustedAdvisorResourceDetail)

-- | Additional information about the identified resource. The exact metadata
-- and its order can be obtained by inspecting the
-- TrustedAdvisorCheckDescription object returned by the call to
-- DescribeTrustedAdvisorChecks. __Metadata__ contains all the data that is
-- shown in the Excel download, even in those cases where the UI shows just
-- summary data.
trustedAdvisorResourceDetail_metadata :: Lens.Lens' TrustedAdvisorResourceDetail [Core.Text]
trustedAdvisorResourceDetail_metadata = Lens.lens (\TrustedAdvisorResourceDetail' {metadata} -> metadata) (\s@TrustedAdvisorResourceDetail' {} a -> s {metadata = a} :: TrustedAdvisorResourceDetail) Core.. Lens._Coerce

instance Core.FromJSON TrustedAdvisorResourceDetail where
  parseJSON =
    Core.withObject
      "TrustedAdvisorResourceDetail"
      ( \x ->
          TrustedAdvisorResourceDetail'
            Core.<$> (x Core..:? "isSuppressed")
            Core.<*> (x Core..:? "region")
            Core.<*> (x Core..: "status")
            Core.<*> (x Core..: "resourceId")
            Core.<*> (x Core..:? "metadata" Core..!= Core.mempty)
      )

instance Core.Hashable TrustedAdvisorResourceDetail

instance Core.NFData TrustedAdvisorResourceDetail
