{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.GetAutomatedDiscoveryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration settings and status of automated sensitive
-- data discovery for an account.
module Amazonka.MacieV2.GetAutomatedDiscoveryConfiguration
  ( -- * Creating a Request
    GetAutomatedDiscoveryConfiguration (..),
    newGetAutomatedDiscoveryConfiguration,

    -- * Destructuring the Response
    GetAutomatedDiscoveryConfigurationResponse (..),
    newGetAutomatedDiscoveryConfigurationResponse,

    -- * Response Lenses
    getAutomatedDiscoveryConfigurationResponse_classificationScopeId,
    getAutomatedDiscoveryConfigurationResponse_disabledAt,
    getAutomatedDiscoveryConfigurationResponse_firstEnabledAt,
    getAutomatedDiscoveryConfigurationResponse_lastUpdatedAt,
    getAutomatedDiscoveryConfigurationResponse_sensitivityInspectionTemplateId,
    getAutomatedDiscoveryConfigurationResponse_status,
    getAutomatedDiscoveryConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAutomatedDiscoveryConfiguration' smart constructor.
data GetAutomatedDiscoveryConfiguration = GetAutomatedDiscoveryConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutomatedDiscoveryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAutomatedDiscoveryConfiguration ::
  GetAutomatedDiscoveryConfiguration
newGetAutomatedDiscoveryConfiguration =
  GetAutomatedDiscoveryConfiguration'

instance
  Core.AWSRequest
    GetAutomatedDiscoveryConfiguration
  where
  type
    AWSResponse GetAutomatedDiscoveryConfiguration =
      GetAutomatedDiscoveryConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutomatedDiscoveryConfigurationResponse'
            Prelude.<$> (x Data..?> "classificationScopeId")
            Prelude.<*> (x Data..?> "disabledAt")
            Prelude.<*> (x Data..?> "firstEnabledAt")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "sensitivityInspectionTemplateId")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAutomatedDiscoveryConfiguration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    GetAutomatedDiscoveryConfiguration
  where
  rnf _ = ()

instance
  Data.ToHeaders
    GetAutomatedDiscoveryConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    GetAutomatedDiscoveryConfiguration
  where
  toPath =
    Prelude.const "/automated-discovery/configuration"

instance
  Data.ToQuery
    GetAutomatedDiscoveryConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAutomatedDiscoveryConfigurationResponse' smart constructor.
data GetAutomatedDiscoveryConfigurationResponse = GetAutomatedDiscoveryConfigurationResponse'
  { -- | The unique identifier for the classification scope that\'s used when
    -- performing automated sensitive data discovery for the account. The
    -- classification scope specifies S3 buckets to exclude from automated
    -- sensitive data discovery.
    classificationScopeId :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when automated
    -- sensitive data discovery was most recently disabled for the account.
    -- This value is null if automated sensitive data discovery wasn\'t enabled
    -- and subsequently disabled for the account.
    disabledAt :: Prelude.Maybe Data.ISO8601,
    -- | The date and time, in UTC and extended ISO 8601 format, when automated
    -- sensitive data discovery was initially enabled for the account. This
    -- value is null if automated sensitive data discovery has never been
    -- enabled for the account.
    firstEnabledAt :: Prelude.Maybe Data.ISO8601,
    -- | The date and time, in UTC and extended ISO 8601 format, when automated
    -- sensitive data discovery was most recently enabled or disabled for the
    -- account.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The unique identifier for the sensitivity inspection template that\'s
    -- used when performing automated sensitive data discovery for the account.
    -- The template specifies which allow lists, custom data identifiers, and
    -- managed data identifiers to use when analyzing data.
    sensitivityInspectionTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the automated sensitive data discovery
    -- configuration for the account. Possible values are: ENABLED, use the
    -- specified settings to perform automated sensitive data discovery
    -- activities for the account; and, DISABLED, don\'t perform automated
    -- sensitive data discovery activities for the account.
    status :: Prelude.Maybe AutomatedDiscoveryStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutomatedDiscoveryConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classificationScopeId', 'getAutomatedDiscoveryConfigurationResponse_classificationScopeId' - The unique identifier for the classification scope that\'s used when
-- performing automated sensitive data discovery for the account. The
-- classification scope specifies S3 buckets to exclude from automated
-- sensitive data discovery.
--
-- 'disabledAt', 'getAutomatedDiscoveryConfigurationResponse_disabledAt' - The date and time, in UTC and extended ISO 8601 format, when automated
-- sensitive data discovery was most recently disabled for the account.
-- This value is null if automated sensitive data discovery wasn\'t enabled
-- and subsequently disabled for the account.
--
-- 'firstEnabledAt', 'getAutomatedDiscoveryConfigurationResponse_firstEnabledAt' - The date and time, in UTC and extended ISO 8601 format, when automated
-- sensitive data discovery was initially enabled for the account. This
-- value is null if automated sensitive data discovery has never been
-- enabled for the account.
--
-- 'lastUpdatedAt', 'getAutomatedDiscoveryConfigurationResponse_lastUpdatedAt' - The date and time, in UTC and extended ISO 8601 format, when automated
-- sensitive data discovery was most recently enabled or disabled for the
-- account.
--
-- 'sensitivityInspectionTemplateId', 'getAutomatedDiscoveryConfigurationResponse_sensitivityInspectionTemplateId' - The unique identifier for the sensitivity inspection template that\'s
-- used when performing automated sensitive data discovery for the account.
-- The template specifies which allow lists, custom data identifiers, and
-- managed data identifiers to use when analyzing data.
--
-- 'status', 'getAutomatedDiscoveryConfigurationResponse_status' - The current status of the automated sensitive data discovery
-- configuration for the account. Possible values are: ENABLED, use the
-- specified settings to perform automated sensitive data discovery
-- activities for the account; and, DISABLED, don\'t perform automated
-- sensitive data discovery activities for the account.
--
-- 'httpStatus', 'getAutomatedDiscoveryConfigurationResponse_httpStatus' - The response's http status code.
newGetAutomatedDiscoveryConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAutomatedDiscoveryConfigurationResponse
newGetAutomatedDiscoveryConfigurationResponse
  pHttpStatus_ =
    GetAutomatedDiscoveryConfigurationResponse'
      { classificationScopeId =
          Prelude.Nothing,
        disabledAt = Prelude.Nothing,
        firstEnabledAt =
          Prelude.Nothing,
        lastUpdatedAt = Prelude.Nothing,
        sensitivityInspectionTemplateId =
          Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique identifier for the classification scope that\'s used when
-- performing automated sensitive data discovery for the account. The
-- classification scope specifies S3 buckets to exclude from automated
-- sensitive data discovery.
getAutomatedDiscoveryConfigurationResponse_classificationScopeId :: Lens.Lens' GetAutomatedDiscoveryConfigurationResponse (Prelude.Maybe Prelude.Text)
getAutomatedDiscoveryConfigurationResponse_classificationScopeId = Lens.lens (\GetAutomatedDiscoveryConfigurationResponse' {classificationScopeId} -> classificationScopeId) (\s@GetAutomatedDiscoveryConfigurationResponse' {} a -> s {classificationScopeId = a} :: GetAutomatedDiscoveryConfigurationResponse)

-- | The date and time, in UTC and extended ISO 8601 format, when automated
-- sensitive data discovery was most recently disabled for the account.
-- This value is null if automated sensitive data discovery wasn\'t enabled
-- and subsequently disabled for the account.
getAutomatedDiscoveryConfigurationResponse_disabledAt :: Lens.Lens' GetAutomatedDiscoveryConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
getAutomatedDiscoveryConfigurationResponse_disabledAt = Lens.lens (\GetAutomatedDiscoveryConfigurationResponse' {disabledAt} -> disabledAt) (\s@GetAutomatedDiscoveryConfigurationResponse' {} a -> s {disabledAt = a} :: GetAutomatedDiscoveryConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time, in UTC and extended ISO 8601 format, when automated
-- sensitive data discovery was initially enabled for the account. This
-- value is null if automated sensitive data discovery has never been
-- enabled for the account.
getAutomatedDiscoveryConfigurationResponse_firstEnabledAt :: Lens.Lens' GetAutomatedDiscoveryConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
getAutomatedDiscoveryConfigurationResponse_firstEnabledAt = Lens.lens (\GetAutomatedDiscoveryConfigurationResponse' {firstEnabledAt} -> firstEnabledAt) (\s@GetAutomatedDiscoveryConfigurationResponse' {} a -> s {firstEnabledAt = a} :: GetAutomatedDiscoveryConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time, in UTC and extended ISO 8601 format, when automated
-- sensitive data discovery was most recently enabled or disabled for the
-- account.
getAutomatedDiscoveryConfigurationResponse_lastUpdatedAt :: Lens.Lens' GetAutomatedDiscoveryConfigurationResponse (Prelude.Maybe Prelude.UTCTime)
getAutomatedDiscoveryConfigurationResponse_lastUpdatedAt = Lens.lens (\GetAutomatedDiscoveryConfigurationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetAutomatedDiscoveryConfigurationResponse' {} a -> s {lastUpdatedAt = a} :: GetAutomatedDiscoveryConfigurationResponse) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the sensitivity inspection template that\'s
-- used when performing automated sensitive data discovery for the account.
-- The template specifies which allow lists, custom data identifiers, and
-- managed data identifiers to use when analyzing data.
getAutomatedDiscoveryConfigurationResponse_sensitivityInspectionTemplateId :: Lens.Lens' GetAutomatedDiscoveryConfigurationResponse (Prelude.Maybe Prelude.Text)
getAutomatedDiscoveryConfigurationResponse_sensitivityInspectionTemplateId = Lens.lens (\GetAutomatedDiscoveryConfigurationResponse' {sensitivityInspectionTemplateId} -> sensitivityInspectionTemplateId) (\s@GetAutomatedDiscoveryConfigurationResponse' {} a -> s {sensitivityInspectionTemplateId = a} :: GetAutomatedDiscoveryConfigurationResponse)

-- | The current status of the automated sensitive data discovery
-- configuration for the account. Possible values are: ENABLED, use the
-- specified settings to perform automated sensitive data discovery
-- activities for the account; and, DISABLED, don\'t perform automated
-- sensitive data discovery activities for the account.
getAutomatedDiscoveryConfigurationResponse_status :: Lens.Lens' GetAutomatedDiscoveryConfigurationResponse (Prelude.Maybe AutomatedDiscoveryStatus)
getAutomatedDiscoveryConfigurationResponse_status = Lens.lens (\GetAutomatedDiscoveryConfigurationResponse' {status} -> status) (\s@GetAutomatedDiscoveryConfigurationResponse' {} a -> s {status = a} :: GetAutomatedDiscoveryConfigurationResponse)

-- | The response's http status code.
getAutomatedDiscoveryConfigurationResponse_httpStatus :: Lens.Lens' GetAutomatedDiscoveryConfigurationResponse Prelude.Int
getAutomatedDiscoveryConfigurationResponse_httpStatus = Lens.lens (\GetAutomatedDiscoveryConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetAutomatedDiscoveryConfigurationResponse' {} a -> s {httpStatus = a} :: GetAutomatedDiscoveryConfigurationResponse)

instance
  Prelude.NFData
    GetAutomatedDiscoveryConfigurationResponse
  where
  rnf GetAutomatedDiscoveryConfigurationResponse' {..} =
    Prelude.rnf classificationScopeId
      `Prelude.seq` Prelude.rnf disabledAt
      `Prelude.seq` Prelude.rnf firstEnabledAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf sensitivityInspectionTemplateId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
