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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptionsLogConfig

-- | configures the CloudWatch Logs to publish for the Elasticsearch domain.
--
-- /See:/ 'newAwsElasticsearchDomainLogPublishingOptions' smart constructor.
data AwsElasticsearchDomainLogPublishingOptions = AwsElasticsearchDomainLogPublishingOptions'
  { auditLogs :: Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig,
    -- | Configures the OpenSearch index logs publishing.
    indexSlowLogs :: Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig,
    -- | Configures the OpenSearch search slow log publishing.
    searchSlowLogs :: Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticsearchDomainLogPublishingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogs', 'awsElasticsearchDomainLogPublishingOptions_auditLogs' - Undocumented member.
--
-- 'indexSlowLogs', 'awsElasticsearchDomainLogPublishingOptions_indexSlowLogs' - Configures the OpenSearch index logs publishing.
--
-- 'searchSlowLogs', 'awsElasticsearchDomainLogPublishingOptions_searchSlowLogs' - Configures the OpenSearch search slow log publishing.
newAwsElasticsearchDomainLogPublishingOptions ::
  AwsElasticsearchDomainLogPublishingOptions
newAwsElasticsearchDomainLogPublishingOptions =
  AwsElasticsearchDomainLogPublishingOptions'
    { auditLogs =
        Prelude.Nothing,
      indexSlowLogs = Prelude.Nothing,
      searchSlowLogs =
        Prelude.Nothing
    }

-- | Undocumented member.
awsElasticsearchDomainLogPublishingOptions_auditLogs :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptions (Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig)
awsElasticsearchDomainLogPublishingOptions_auditLogs = Lens.lens (\AwsElasticsearchDomainLogPublishingOptions' {auditLogs} -> auditLogs) (\s@AwsElasticsearchDomainLogPublishingOptions' {} a -> s {auditLogs = a} :: AwsElasticsearchDomainLogPublishingOptions)

-- | Configures the OpenSearch index logs publishing.
awsElasticsearchDomainLogPublishingOptions_indexSlowLogs :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptions (Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig)
awsElasticsearchDomainLogPublishingOptions_indexSlowLogs = Lens.lens (\AwsElasticsearchDomainLogPublishingOptions' {indexSlowLogs} -> indexSlowLogs) (\s@AwsElasticsearchDomainLogPublishingOptions' {} a -> s {indexSlowLogs = a} :: AwsElasticsearchDomainLogPublishingOptions)

-- | Configures the OpenSearch search slow log publishing.
awsElasticsearchDomainLogPublishingOptions_searchSlowLogs :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptions (Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig)
awsElasticsearchDomainLogPublishingOptions_searchSlowLogs = Lens.lens (\AwsElasticsearchDomainLogPublishingOptions' {searchSlowLogs} -> searchSlowLogs) (\s@AwsElasticsearchDomainLogPublishingOptions' {} a -> s {searchSlowLogs = a} :: AwsElasticsearchDomainLogPublishingOptions)

instance
  Data.FromJSON
    AwsElasticsearchDomainLogPublishingOptions
  where
  parseJSON =
    Data.withObject
      "AwsElasticsearchDomainLogPublishingOptions"
      ( \x ->
          AwsElasticsearchDomainLogPublishingOptions'
            Prelude.<$> (x Data..:? "AuditLogs")
            Prelude.<*> (x Data..:? "IndexSlowLogs")
            Prelude.<*> (x Data..:? "SearchSlowLogs")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainLogPublishingOptions
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainLogPublishingOptions' {..} =
      _salt
        `Prelude.hashWithSalt` auditLogs
        `Prelude.hashWithSalt` indexSlowLogs
        `Prelude.hashWithSalt` searchSlowLogs

instance
  Prelude.NFData
    AwsElasticsearchDomainLogPublishingOptions
  where
  rnf AwsElasticsearchDomainLogPublishingOptions' {..} =
    Prelude.rnf auditLogs `Prelude.seq`
      Prelude.rnf indexSlowLogs `Prelude.seq`
        Prelude.rnf searchSlowLogs

instance
  Data.ToJSON
    AwsElasticsearchDomainLogPublishingOptions
  where
  toJSON
    AwsElasticsearchDomainLogPublishingOptions' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AuditLogs" Data..=) Prelude.<$> auditLogs,
              ("IndexSlowLogs" Data..=) Prelude.<$> indexSlowLogs,
              ("SearchSlowLogs" Data..=)
                Prelude.<$> searchSlowLogs
            ]
        )
