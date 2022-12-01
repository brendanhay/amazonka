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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptionsLogConfig

-- | configures the CloudWatch Logs to publish for the Elasticsearch domain.
--
-- /See:/ 'newAwsElasticsearchDomainLogPublishingOptions' smart constructor.
data AwsElasticsearchDomainLogPublishingOptions = AwsElasticsearchDomainLogPublishingOptions'
  { -- | Configures the OpenSearch index logs publishing.
    indexSlowLogs :: Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig,
    auditLogs :: Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig,
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
-- 'indexSlowLogs', 'awsElasticsearchDomainLogPublishingOptions_indexSlowLogs' - Configures the OpenSearch index logs publishing.
--
-- 'auditLogs', 'awsElasticsearchDomainLogPublishingOptions_auditLogs' - Undocumented member.
--
-- 'searchSlowLogs', 'awsElasticsearchDomainLogPublishingOptions_searchSlowLogs' - Configures the OpenSearch search slow log publishing.
newAwsElasticsearchDomainLogPublishingOptions ::
  AwsElasticsearchDomainLogPublishingOptions
newAwsElasticsearchDomainLogPublishingOptions =
  AwsElasticsearchDomainLogPublishingOptions'
    { indexSlowLogs =
        Prelude.Nothing,
      auditLogs = Prelude.Nothing,
      searchSlowLogs =
        Prelude.Nothing
    }

-- | Configures the OpenSearch index logs publishing.
awsElasticsearchDomainLogPublishingOptions_indexSlowLogs :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptions (Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig)
awsElasticsearchDomainLogPublishingOptions_indexSlowLogs = Lens.lens (\AwsElasticsearchDomainLogPublishingOptions' {indexSlowLogs} -> indexSlowLogs) (\s@AwsElasticsearchDomainLogPublishingOptions' {} a -> s {indexSlowLogs = a} :: AwsElasticsearchDomainLogPublishingOptions)

-- | Undocumented member.
awsElasticsearchDomainLogPublishingOptions_auditLogs :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptions (Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig)
awsElasticsearchDomainLogPublishingOptions_auditLogs = Lens.lens (\AwsElasticsearchDomainLogPublishingOptions' {auditLogs} -> auditLogs) (\s@AwsElasticsearchDomainLogPublishingOptions' {} a -> s {auditLogs = a} :: AwsElasticsearchDomainLogPublishingOptions)

-- | Configures the OpenSearch search slow log publishing.
awsElasticsearchDomainLogPublishingOptions_searchSlowLogs :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptions (Prelude.Maybe AwsElasticsearchDomainLogPublishingOptionsLogConfig)
awsElasticsearchDomainLogPublishingOptions_searchSlowLogs = Lens.lens (\AwsElasticsearchDomainLogPublishingOptions' {searchSlowLogs} -> searchSlowLogs) (\s@AwsElasticsearchDomainLogPublishingOptions' {} a -> s {searchSlowLogs = a} :: AwsElasticsearchDomainLogPublishingOptions)

instance
  Core.FromJSON
    AwsElasticsearchDomainLogPublishingOptions
  where
  parseJSON =
    Core.withObject
      "AwsElasticsearchDomainLogPublishingOptions"
      ( \x ->
          AwsElasticsearchDomainLogPublishingOptions'
            Prelude.<$> (x Core..:? "IndexSlowLogs")
              Prelude.<*> (x Core..:? "AuditLogs")
              Prelude.<*> (x Core..:? "SearchSlowLogs")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainLogPublishingOptions
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainLogPublishingOptions' {..} =
      _salt `Prelude.hashWithSalt` indexSlowLogs
        `Prelude.hashWithSalt` auditLogs
        `Prelude.hashWithSalt` searchSlowLogs

instance
  Prelude.NFData
    AwsElasticsearchDomainLogPublishingOptions
  where
  rnf AwsElasticsearchDomainLogPublishingOptions' {..} =
    Prelude.rnf indexSlowLogs
      `Prelude.seq` Prelude.rnf auditLogs
      `Prelude.seq` Prelude.rnf searchSlowLogs

instance
  Core.ToJSON
    AwsElasticsearchDomainLogPublishingOptions
  where
  toJSON
    AwsElasticsearchDomainLogPublishingOptions' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("IndexSlowLogs" Core..=) Prelude.<$> indexSlowLogs,
              ("AuditLogs" Core..=) Prelude.<$> auditLogs,
              ("SearchSlowLogs" Core..=)
                Prelude.<$> searchSlowLogs
            ]
        )
