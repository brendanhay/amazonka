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
-- Module      : Amazonka.Support.Types.TrustedAdvisorResourceDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.TrustedAdvisorResourceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a resource identified by a Trusted Advisor
-- check.
--
-- /See:/ 'newTrustedAdvisorResourceDetail' smart constructor.
data TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail'
  { -- | Specifies whether the Amazon Web Services resource was ignored by
    -- Trusted Advisor because it was marked as suppressed by the user.
    isSuppressed :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services Region in which the identified resource is
    -- located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The status code for the resource identified in the Trusted Advisor
    -- check.
    status :: Prelude.Text,
    -- | The unique identifier for the identified resource.
    resourceId :: Prelude.Text,
    -- | Additional information about the identified resource. The exact metadata
    -- and its order can be obtained by inspecting the
    -- TrustedAdvisorCheckDescription object returned by the call to
    -- DescribeTrustedAdvisorChecks. __Metadata__ contains all the data that is
    -- shown in the Excel download, even in those cases where the UI shows just
    -- summary data.
    metadata :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustedAdvisorResourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isSuppressed', 'trustedAdvisorResourceDetail_isSuppressed' - Specifies whether the Amazon Web Services resource was ignored by
-- Trusted Advisor because it was marked as suppressed by the user.
--
-- 'region', 'trustedAdvisorResourceDetail_region' - The Amazon Web Services Region in which the identified resource is
-- located.
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
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  TrustedAdvisorResourceDetail
newTrustedAdvisorResourceDetail pStatus_ pResourceId_ =
  TrustedAdvisorResourceDetail'
    { isSuppressed =
        Prelude.Nothing,
      region = Prelude.Nothing,
      status = pStatus_,
      resourceId = pResourceId_,
      metadata = Prelude.mempty
    }

-- | Specifies whether the Amazon Web Services resource was ignored by
-- Trusted Advisor because it was marked as suppressed by the user.
trustedAdvisorResourceDetail_isSuppressed :: Lens.Lens' TrustedAdvisorResourceDetail (Prelude.Maybe Prelude.Bool)
trustedAdvisorResourceDetail_isSuppressed = Lens.lens (\TrustedAdvisorResourceDetail' {isSuppressed} -> isSuppressed) (\s@TrustedAdvisorResourceDetail' {} a -> s {isSuppressed = a} :: TrustedAdvisorResourceDetail)

-- | The Amazon Web Services Region in which the identified resource is
-- located.
trustedAdvisorResourceDetail_region :: Lens.Lens' TrustedAdvisorResourceDetail (Prelude.Maybe Prelude.Text)
trustedAdvisorResourceDetail_region = Lens.lens (\TrustedAdvisorResourceDetail' {region} -> region) (\s@TrustedAdvisorResourceDetail' {} a -> s {region = a} :: TrustedAdvisorResourceDetail)

-- | The status code for the resource identified in the Trusted Advisor
-- check.
trustedAdvisorResourceDetail_status :: Lens.Lens' TrustedAdvisorResourceDetail Prelude.Text
trustedAdvisorResourceDetail_status = Lens.lens (\TrustedAdvisorResourceDetail' {status} -> status) (\s@TrustedAdvisorResourceDetail' {} a -> s {status = a} :: TrustedAdvisorResourceDetail)

-- | The unique identifier for the identified resource.
trustedAdvisorResourceDetail_resourceId :: Lens.Lens' TrustedAdvisorResourceDetail Prelude.Text
trustedAdvisorResourceDetail_resourceId = Lens.lens (\TrustedAdvisorResourceDetail' {resourceId} -> resourceId) (\s@TrustedAdvisorResourceDetail' {} a -> s {resourceId = a} :: TrustedAdvisorResourceDetail)

-- | Additional information about the identified resource. The exact metadata
-- and its order can be obtained by inspecting the
-- TrustedAdvisorCheckDescription object returned by the call to
-- DescribeTrustedAdvisorChecks. __Metadata__ contains all the data that is
-- shown in the Excel download, even in those cases where the UI shows just
-- summary data.
trustedAdvisorResourceDetail_metadata :: Lens.Lens' TrustedAdvisorResourceDetail [Prelude.Text]
trustedAdvisorResourceDetail_metadata = Lens.lens (\TrustedAdvisorResourceDetail' {metadata} -> metadata) (\s@TrustedAdvisorResourceDetail' {} a -> s {metadata = a} :: TrustedAdvisorResourceDetail) Prelude.. Lens.coerced

instance Data.FromJSON TrustedAdvisorResourceDetail where
  parseJSON =
    Data.withObject
      "TrustedAdvisorResourceDetail"
      ( \x ->
          TrustedAdvisorResourceDetail'
            Prelude.<$> (x Data..:? "isSuppressed")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "resourceId")
            Prelude.<*> (x Data..:? "metadata" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    TrustedAdvisorResourceDetail
  where
  hashWithSalt _salt TrustedAdvisorResourceDetail' {..} =
    _salt
      `Prelude.hashWithSalt` isSuppressed
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` metadata

instance Prelude.NFData TrustedAdvisorResourceDetail where
  rnf TrustedAdvisorResourceDetail' {..} =
    Prelude.rnf isSuppressed `Prelude.seq`
      Prelude.rnf region `Prelude.seq`
        Prelude.rnf status `Prelude.seq`
          Prelude.rnf resourceId `Prelude.seq`
            Prelude.rnf metadata
