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
-- Module      : Amazonka.SecurityLake.CreateCustomLogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a third-party custom source in Amazon Security Lake, from the
-- Amazon Web Services Region where you want to create a custom source.
-- Security Lake can collect logs and events from third-party custom
-- sources. After creating the appropriate IAM role to invoke Glue crawler,
-- use this API to add a custom source name in Security Lake. This
-- operation creates a partition in the Amazon S3 bucket for Security Lake
-- as the target location for log files from the custom source. In
-- addition, this operation also creates an associated Glue table and an
-- Glue crawler.
module Amazonka.SecurityLake.CreateCustomLogSource
  ( -- * Creating a Request
    CreateCustomLogSource (..),
    newCreateCustomLogSource,

    -- * Request Lenses
    createCustomLogSource_configuration,
    createCustomLogSource_eventClasses,
    createCustomLogSource_sourceVersion,
    createCustomLogSource_sourceName,

    -- * Destructuring the Response
    CreateCustomLogSourceResponse (..),
    newCreateCustomLogSourceResponse,

    -- * Response Lenses
    createCustomLogSourceResponse_source,
    createCustomLogSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateCustomLogSource' smart constructor.
data CreateCustomLogSource = CreateCustomLogSource'
  { -- | The configuration for the third-party custom source.
    configuration :: Prelude.Maybe CustomLogSourceConfiguration,
    -- | The Open Cybersecurity Schema Framework (OCSF) event classes which
    -- describes the type of data that the custom source will send to Security
    -- Lake. The supported event classes are:
    --
    -- -   @ACCESS_ACTIVITY@
    --
    -- -   @FILE_ACTIVITY@
    --
    -- -   @KERNEL_ACTIVITY@
    --
    -- -   @KERNEL_EXTENSION@
    --
    -- -   @MEMORY_ACTIVITY@
    --
    -- -   @MODULE_ACTIVITY@
    --
    -- -   @PROCESS_ACTIVITY@
    --
    -- -   @REGISTRY_KEY_ACTIVITY@
    --
    -- -   @REGISTRY_VALUE_ACTIVITY@
    --
    -- -   @RESOURCE_ACTIVITY@
    --
    -- -   @SCHEDULED_JOB_ACTIVITY@
    --
    -- -   @SECURITY_FINDING@
    --
    -- -   @ACCOUNT_CHANGE@
    --
    -- -   @AUTHENTICATION@
    --
    -- -   @AUTHORIZATION@
    --
    -- -   @ENTITY_MANAGEMENT_AUDIT@
    --
    -- -   @DHCP_ACTIVITY@
    --
    -- -   @NETWORK_ACTIVITY@
    --
    -- -   @DNS_ACTIVITY@
    --
    -- -   @FTP_ACTIVITY@
    --
    -- -   @HTTP_ACTIVITY@
    --
    -- -   @RDP_ACTIVITY@
    --
    -- -   @SMB_ACTIVITY@
    --
    -- -   @SSH_ACTIVITY@
    --
    -- -   @CONFIG_STATE@
    --
    -- -   @INVENTORY_INFO@
    --
    -- -   @EMAIL_ACTIVITY@
    --
    -- -   @API_ACTIVITY@
    --
    -- -   @CLOUD_API@
    eventClasses :: Prelude.Maybe [Prelude.Text],
    -- | Specify the source version for the third-party custom source, to limit
    -- log collection to a specific version of custom data source.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | Specify the name for a third-party custom source. This must be a
    -- Regionally unique value.
    sourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomLogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'createCustomLogSource_configuration' - The configuration for the third-party custom source.
--
-- 'eventClasses', 'createCustomLogSource_eventClasses' - The Open Cybersecurity Schema Framework (OCSF) event classes which
-- describes the type of data that the custom source will send to Security
-- Lake. The supported event classes are:
--
-- -   @ACCESS_ACTIVITY@
--
-- -   @FILE_ACTIVITY@
--
-- -   @KERNEL_ACTIVITY@
--
-- -   @KERNEL_EXTENSION@
--
-- -   @MEMORY_ACTIVITY@
--
-- -   @MODULE_ACTIVITY@
--
-- -   @PROCESS_ACTIVITY@
--
-- -   @REGISTRY_KEY_ACTIVITY@
--
-- -   @REGISTRY_VALUE_ACTIVITY@
--
-- -   @RESOURCE_ACTIVITY@
--
-- -   @SCHEDULED_JOB_ACTIVITY@
--
-- -   @SECURITY_FINDING@
--
-- -   @ACCOUNT_CHANGE@
--
-- -   @AUTHENTICATION@
--
-- -   @AUTHORIZATION@
--
-- -   @ENTITY_MANAGEMENT_AUDIT@
--
-- -   @DHCP_ACTIVITY@
--
-- -   @NETWORK_ACTIVITY@
--
-- -   @DNS_ACTIVITY@
--
-- -   @FTP_ACTIVITY@
--
-- -   @HTTP_ACTIVITY@
--
-- -   @RDP_ACTIVITY@
--
-- -   @SMB_ACTIVITY@
--
-- -   @SSH_ACTIVITY@
--
-- -   @CONFIG_STATE@
--
-- -   @INVENTORY_INFO@
--
-- -   @EMAIL_ACTIVITY@
--
-- -   @API_ACTIVITY@
--
-- -   @CLOUD_API@
--
-- 'sourceVersion', 'createCustomLogSource_sourceVersion' - Specify the source version for the third-party custom source, to limit
-- log collection to a specific version of custom data source.
--
-- 'sourceName', 'createCustomLogSource_sourceName' - Specify the name for a third-party custom source. This must be a
-- Regionally unique value.
newCreateCustomLogSource ::
  -- | 'sourceName'
  Prelude.Text ->
  CreateCustomLogSource
newCreateCustomLogSource pSourceName_ =
  CreateCustomLogSource'
    { configuration =
        Prelude.Nothing,
      eventClasses = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      sourceName = pSourceName_
    }

-- | The configuration for the third-party custom source.
createCustomLogSource_configuration :: Lens.Lens' CreateCustomLogSource (Prelude.Maybe CustomLogSourceConfiguration)
createCustomLogSource_configuration = Lens.lens (\CreateCustomLogSource' {configuration} -> configuration) (\s@CreateCustomLogSource' {} a -> s {configuration = a} :: CreateCustomLogSource)

-- | The Open Cybersecurity Schema Framework (OCSF) event classes which
-- describes the type of data that the custom source will send to Security
-- Lake. The supported event classes are:
--
-- -   @ACCESS_ACTIVITY@
--
-- -   @FILE_ACTIVITY@
--
-- -   @KERNEL_ACTIVITY@
--
-- -   @KERNEL_EXTENSION@
--
-- -   @MEMORY_ACTIVITY@
--
-- -   @MODULE_ACTIVITY@
--
-- -   @PROCESS_ACTIVITY@
--
-- -   @REGISTRY_KEY_ACTIVITY@
--
-- -   @REGISTRY_VALUE_ACTIVITY@
--
-- -   @RESOURCE_ACTIVITY@
--
-- -   @SCHEDULED_JOB_ACTIVITY@
--
-- -   @SECURITY_FINDING@
--
-- -   @ACCOUNT_CHANGE@
--
-- -   @AUTHENTICATION@
--
-- -   @AUTHORIZATION@
--
-- -   @ENTITY_MANAGEMENT_AUDIT@
--
-- -   @DHCP_ACTIVITY@
--
-- -   @NETWORK_ACTIVITY@
--
-- -   @DNS_ACTIVITY@
--
-- -   @FTP_ACTIVITY@
--
-- -   @HTTP_ACTIVITY@
--
-- -   @RDP_ACTIVITY@
--
-- -   @SMB_ACTIVITY@
--
-- -   @SSH_ACTIVITY@
--
-- -   @CONFIG_STATE@
--
-- -   @INVENTORY_INFO@
--
-- -   @EMAIL_ACTIVITY@
--
-- -   @API_ACTIVITY@
--
-- -   @CLOUD_API@
createCustomLogSource_eventClasses :: Lens.Lens' CreateCustomLogSource (Prelude.Maybe [Prelude.Text])
createCustomLogSource_eventClasses = Lens.lens (\CreateCustomLogSource' {eventClasses} -> eventClasses) (\s@CreateCustomLogSource' {} a -> s {eventClasses = a} :: CreateCustomLogSource) Prelude.. Lens.mapping Lens.coerced

-- | Specify the source version for the third-party custom source, to limit
-- log collection to a specific version of custom data source.
createCustomLogSource_sourceVersion :: Lens.Lens' CreateCustomLogSource (Prelude.Maybe Prelude.Text)
createCustomLogSource_sourceVersion = Lens.lens (\CreateCustomLogSource' {sourceVersion} -> sourceVersion) (\s@CreateCustomLogSource' {} a -> s {sourceVersion = a} :: CreateCustomLogSource)

-- | Specify the name for a third-party custom source. This must be a
-- Regionally unique value.
createCustomLogSource_sourceName :: Lens.Lens' CreateCustomLogSource Prelude.Text
createCustomLogSource_sourceName = Lens.lens (\CreateCustomLogSource' {sourceName} -> sourceName) (\s@CreateCustomLogSource' {} a -> s {sourceName = a} :: CreateCustomLogSource)

instance Core.AWSRequest CreateCustomLogSource where
  type
    AWSResponse CreateCustomLogSource =
      CreateCustomLogSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomLogSourceResponse'
            Prelude.<$> (x Data..?> "source")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomLogSource where
  hashWithSalt _salt CreateCustomLogSource' {..} =
    _salt
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` eventClasses
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` sourceName

instance Prelude.NFData CreateCustomLogSource where
  rnf CreateCustomLogSource' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf eventClasses
      `Prelude.seq` Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf sourceName

instance Data.ToHeaders CreateCustomLogSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCustomLogSource where
  toJSON CreateCustomLogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configuration" Data..=) Prelude.<$> configuration,
            ("eventClasses" Data..=) Prelude.<$> eventClasses,
            ("sourceVersion" Data..=) Prelude.<$> sourceVersion,
            Prelude.Just ("sourceName" Data..= sourceName)
          ]
      )

instance Data.ToPath CreateCustomLogSource where
  toPath =
    Prelude.const "/v1/datalake/logsources/custom"

instance Data.ToQuery CreateCustomLogSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomLogSourceResponse' smart constructor.
data CreateCustomLogSourceResponse = CreateCustomLogSourceResponse'
  { -- | The created third-party custom source.
    source :: Prelude.Maybe CustomLogSourceResource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomLogSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'createCustomLogSourceResponse_source' - The created third-party custom source.
--
-- 'httpStatus', 'createCustomLogSourceResponse_httpStatus' - The response's http status code.
newCreateCustomLogSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomLogSourceResponse
newCreateCustomLogSourceResponse pHttpStatus_ =
  CreateCustomLogSourceResponse'
    { source =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The created third-party custom source.
createCustomLogSourceResponse_source :: Lens.Lens' CreateCustomLogSourceResponse (Prelude.Maybe CustomLogSourceResource)
createCustomLogSourceResponse_source = Lens.lens (\CreateCustomLogSourceResponse' {source} -> source) (\s@CreateCustomLogSourceResponse' {} a -> s {source = a} :: CreateCustomLogSourceResponse)

-- | The response's http status code.
createCustomLogSourceResponse_httpStatus :: Lens.Lens' CreateCustomLogSourceResponse Prelude.Int
createCustomLogSourceResponse_httpStatus = Lens.lens (\CreateCustomLogSourceResponse' {httpStatus} -> httpStatus) (\s@CreateCustomLogSourceResponse' {} a -> s {httpStatus = a} :: CreateCustomLogSourceResponse)

instance Prelude.NFData CreateCustomLogSourceResponse where
  rnf CreateCustomLogSourceResponse' {..} =
    Prelude.rnf source
      `Prelude.seq` Prelude.rnf httpStatus
