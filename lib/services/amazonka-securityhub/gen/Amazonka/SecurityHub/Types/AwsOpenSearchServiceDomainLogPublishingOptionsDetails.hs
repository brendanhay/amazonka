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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOption

-- | Configures the CloudWatch Logs to publish for the OpenSearch domain.
--
-- /See:/ 'newAwsOpenSearchServiceDomainLogPublishingOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainLogPublishingOptionsDetails = AwsOpenSearchServiceDomainLogPublishingOptionsDetails'
  { -- | Configures the OpenSearch audit logs publishing.
    auditLogs :: Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption,
    -- | Configures the OpenSearch index logs publishing.
    indexSlowLogs :: Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption,
    -- | Configures the OpenSearch search slow log publishing.
    searchSlowLogs :: Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainLogPublishingOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogs', 'awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs' - Configures the OpenSearch audit logs publishing.
--
-- 'indexSlowLogs', 'awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs' - Configures the OpenSearch index logs publishing.
--
-- 'searchSlowLogs', 'awsOpenSearchServiceDomainLogPublishingOptionsDetails_searchSlowLogs' - Configures the OpenSearch search slow log publishing.
newAwsOpenSearchServiceDomainLogPublishingOptionsDetails ::
  AwsOpenSearchServiceDomainLogPublishingOptionsDetails
newAwsOpenSearchServiceDomainLogPublishingOptionsDetails =
  AwsOpenSearchServiceDomainLogPublishingOptionsDetails'
    { auditLogs =
        Prelude.Nothing,
      indexSlowLogs =
        Prelude.Nothing,
      searchSlowLogs =
        Prelude.Nothing
    }

-- | Configures the OpenSearch audit logs publishing.
awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOptionsDetails (Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption)
awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {auditLogs} -> auditLogs) (\s@AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {} a -> s {auditLogs = a} :: AwsOpenSearchServiceDomainLogPublishingOptionsDetails)

-- | Configures the OpenSearch index logs publishing.
awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOptionsDetails (Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption)
awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {indexSlowLogs} -> indexSlowLogs) (\s@AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {} a -> s {indexSlowLogs = a} :: AwsOpenSearchServiceDomainLogPublishingOptionsDetails)

-- | Configures the OpenSearch search slow log publishing.
awsOpenSearchServiceDomainLogPublishingOptionsDetails_searchSlowLogs :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOptionsDetails (Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption)
awsOpenSearchServiceDomainLogPublishingOptionsDetails_searchSlowLogs = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {searchSlowLogs} -> searchSlowLogs) (\s@AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {} a -> s {searchSlowLogs = a} :: AwsOpenSearchServiceDomainLogPublishingOptionsDetails)

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainLogPublishingOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainLogPublishingOptionsDetails'
            Prelude.<$> (x Data..:? "AuditLogs")
            Prelude.<*> (x Data..:? "IndexSlowLogs")
            Prelude.<*> (x Data..:? "SearchSlowLogs")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` auditLogs
        `Prelude.hashWithSalt` indexSlowLogs
        `Prelude.hashWithSalt` searchSlowLogs

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails
  where
  rnf
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {..} =
      Prelude.rnf auditLogs
        `Prelude.seq` Prelude.rnf indexSlowLogs
        `Prelude.seq` Prelude.rnf searchSlowLogs

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AuditLogs" Data..=) Prelude.<$> auditLogs,
              ("IndexSlowLogs" Data..=) Prelude.<$> indexSlowLogs,
              ("SearchSlowLogs" Data..=)
                Prelude.<$> searchSlowLogs
            ]
        )
