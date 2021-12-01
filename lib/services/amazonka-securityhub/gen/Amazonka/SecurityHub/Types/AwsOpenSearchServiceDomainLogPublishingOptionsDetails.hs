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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOption

-- | Configures the CloudWatch Logs to publish for the OpenSearch domain.
--
-- /See:/ 'newAwsOpenSearchServiceDomainLogPublishingOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainLogPublishingOptionsDetails = AwsOpenSearchServiceDomainLogPublishingOptionsDetails'
  { -- | Configures the OpenSearch index logs publishing.
    indexSlowLogs :: Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption,
    -- | Configures the OpenSearch search slow log publishing.
    searchSlowLogs :: Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption,
    -- | Configures the OpenSearch audit logs publishing.
    auditLogs :: Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption
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
-- 'indexSlowLogs', 'awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs' - Configures the OpenSearch index logs publishing.
--
-- 'searchSlowLogs', 'awsOpenSearchServiceDomainLogPublishingOptionsDetails_searchSlowLogs' - Configures the OpenSearch search slow log publishing.
--
-- 'auditLogs', 'awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs' - Configures the OpenSearch audit logs publishing.
newAwsOpenSearchServiceDomainLogPublishingOptionsDetails ::
  AwsOpenSearchServiceDomainLogPublishingOptionsDetails
newAwsOpenSearchServiceDomainLogPublishingOptionsDetails =
  AwsOpenSearchServiceDomainLogPublishingOptionsDetails'
    { indexSlowLogs =
        Prelude.Nothing,
      searchSlowLogs =
        Prelude.Nothing,
      auditLogs =
        Prelude.Nothing
    }

-- | Configures the OpenSearch index logs publishing.
awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOptionsDetails (Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption)
awsOpenSearchServiceDomainLogPublishingOptionsDetails_indexSlowLogs = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {indexSlowLogs} -> indexSlowLogs) (\s@AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {} a -> s {indexSlowLogs = a} :: AwsOpenSearchServiceDomainLogPublishingOptionsDetails)

-- | Configures the OpenSearch search slow log publishing.
awsOpenSearchServiceDomainLogPublishingOptionsDetails_searchSlowLogs :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOptionsDetails (Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption)
awsOpenSearchServiceDomainLogPublishingOptionsDetails_searchSlowLogs = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {searchSlowLogs} -> searchSlowLogs) (\s@AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {} a -> s {searchSlowLogs = a} :: AwsOpenSearchServiceDomainLogPublishingOptionsDetails)

-- | Configures the OpenSearch audit logs publishing.
awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOptionsDetails (Prelude.Maybe AwsOpenSearchServiceDomainLogPublishingOption)
awsOpenSearchServiceDomainLogPublishingOptionsDetails_auditLogs = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {auditLogs} -> auditLogs) (\s@AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {} a -> s {auditLogs = a} :: AwsOpenSearchServiceDomainLogPublishingOptionsDetails)

instance
  Core.FromJSON
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails
  where
  parseJSON =
    Core.withObject
      "AwsOpenSearchServiceDomainLogPublishingOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainLogPublishingOptionsDetails'
            Prelude.<$> (x Core..:? "IndexSlowLogs")
              Prelude.<*> (x Core..:? "SearchSlowLogs")
              Prelude.<*> (x Core..:? "AuditLogs")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails
  where
  hashWithSalt
    salt'
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {..} =
      salt' `Prelude.hashWithSalt` auditLogs
        `Prelude.hashWithSalt` searchSlowLogs
        `Prelude.hashWithSalt` indexSlowLogs

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails
  where
  rnf
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {..} =
      Prelude.rnf indexSlowLogs
        `Prelude.seq` Prelude.rnf auditLogs
        `Prelude.seq` Prelude.rnf searchSlowLogs

instance
  Core.ToJSON
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainLogPublishingOptionsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("IndexSlowLogs" Core..=) Prelude.<$> indexSlowLogs,
              ("SearchSlowLogs" Core..=)
                Prelude.<$> searchSlowLogs,
              ("AuditLogs" Core..=) Prelude.<$> auditLogs
            ]
        )
