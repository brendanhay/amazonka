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
-- Module      : Amazonka.Config.StartRemediationExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an on-demand remediation for the specified Config rules against the
-- last known remediation configuration. It runs an execution against the
-- current state of your resources. Remediation execution is asynchronous.
--
-- You can specify up to 100 resource keys per request. An existing
-- StartRemediationExecution call for the specified resource keys must
-- complete before you can call the API again.
module Amazonka.Config.StartRemediationExecution
  ( -- * Creating a Request
    StartRemediationExecution (..),
    newStartRemediationExecution,

    -- * Request Lenses
    startRemediationExecution_configRuleName,
    startRemediationExecution_resourceKeys,

    -- * Destructuring the Response
    StartRemediationExecutionResponse (..),
    newStartRemediationExecutionResponse,

    -- * Response Lenses
    startRemediationExecutionResponse_failedItems,
    startRemediationExecutionResponse_failureMessage,
    startRemediationExecutionResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartRemediationExecution' smart constructor.
data StartRemediationExecution = StartRemediationExecution'
  { -- | The list of names of Config rules that you want to run remediation
    -- execution for.
    configRuleName :: Prelude.Text,
    -- | A list of resource keys to be processed with the current request. Each
    -- element in the list consists of the resource type and resource ID.
    resourceKeys :: Prelude.NonEmpty ResourceKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRemediationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'startRemediationExecution_configRuleName' - The list of names of Config rules that you want to run remediation
-- execution for.
--
-- 'resourceKeys', 'startRemediationExecution_resourceKeys' - A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
newStartRemediationExecution ::
  -- | 'configRuleName'
  Prelude.Text ->
  -- | 'resourceKeys'
  Prelude.NonEmpty ResourceKey ->
  StartRemediationExecution
newStartRemediationExecution
  pConfigRuleName_
  pResourceKeys_ =
    StartRemediationExecution'
      { configRuleName =
          pConfigRuleName_,
        resourceKeys =
          Lens.coerced Lens.# pResourceKeys_
      }

-- | The list of names of Config rules that you want to run remediation
-- execution for.
startRemediationExecution_configRuleName :: Lens.Lens' StartRemediationExecution Prelude.Text
startRemediationExecution_configRuleName = Lens.lens (\StartRemediationExecution' {configRuleName} -> configRuleName) (\s@StartRemediationExecution' {} a -> s {configRuleName = a} :: StartRemediationExecution)

-- | A list of resource keys to be processed with the current request. Each
-- element in the list consists of the resource type and resource ID.
startRemediationExecution_resourceKeys :: Lens.Lens' StartRemediationExecution (Prelude.NonEmpty ResourceKey)
startRemediationExecution_resourceKeys = Lens.lens (\StartRemediationExecution' {resourceKeys} -> resourceKeys) (\s@StartRemediationExecution' {} a -> s {resourceKeys = a} :: StartRemediationExecution) Prelude.. Lens.coerced

instance Core.AWSRequest StartRemediationExecution where
  type
    AWSResponse StartRemediationExecution =
      StartRemediationExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRemediationExecutionResponse'
            Prelude.<$> (x Core..?> "FailedItems")
            Prelude.<*> (x Core..?> "FailureMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartRemediationExecution where
  hashWithSalt _salt StartRemediationExecution' {..} =
    _salt `Prelude.hashWithSalt` configRuleName
      `Prelude.hashWithSalt` resourceKeys

instance Prelude.NFData StartRemediationExecution where
  rnf StartRemediationExecution' {..} =
    Prelude.rnf configRuleName
      `Prelude.seq` Prelude.rnf resourceKeys

instance Core.ToHeaders StartRemediationExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.StartRemediationExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartRemediationExecution where
  toJSON StartRemediationExecution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConfigRuleName" Core..= configRuleName),
            Prelude.Just ("ResourceKeys" Core..= resourceKeys)
          ]
      )

instance Core.ToPath StartRemediationExecution where
  toPath = Prelude.const "/"

instance Core.ToQuery StartRemediationExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartRemediationExecutionResponse' smart constructor.
data StartRemediationExecutionResponse = StartRemediationExecutionResponse'
  { -- | For resources that have failed to start execution, the API returns a
    -- resource key object.
    failedItems :: Prelude.Maybe (Prelude.NonEmpty ResourceKey),
    -- | Returns a failure message. For example, the resource is already
    -- compliant.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRemediationExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedItems', 'startRemediationExecutionResponse_failedItems' - For resources that have failed to start execution, the API returns a
-- resource key object.
--
-- 'failureMessage', 'startRemediationExecutionResponse_failureMessage' - Returns a failure message. For example, the resource is already
-- compliant.
--
-- 'httpStatus', 'startRemediationExecutionResponse_httpStatus' - The response's http status code.
newStartRemediationExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartRemediationExecutionResponse
newStartRemediationExecutionResponse pHttpStatus_ =
  StartRemediationExecutionResponse'
    { failedItems =
        Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | For resources that have failed to start execution, the API returns a
-- resource key object.
startRemediationExecutionResponse_failedItems :: Lens.Lens' StartRemediationExecutionResponse (Prelude.Maybe (Prelude.NonEmpty ResourceKey))
startRemediationExecutionResponse_failedItems = Lens.lens (\StartRemediationExecutionResponse' {failedItems} -> failedItems) (\s@StartRemediationExecutionResponse' {} a -> s {failedItems = a} :: StartRemediationExecutionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns a failure message. For example, the resource is already
-- compliant.
startRemediationExecutionResponse_failureMessage :: Lens.Lens' StartRemediationExecutionResponse (Prelude.Maybe Prelude.Text)
startRemediationExecutionResponse_failureMessage = Lens.lens (\StartRemediationExecutionResponse' {failureMessage} -> failureMessage) (\s@StartRemediationExecutionResponse' {} a -> s {failureMessage = a} :: StartRemediationExecutionResponse)

-- | The response's http status code.
startRemediationExecutionResponse_httpStatus :: Lens.Lens' StartRemediationExecutionResponse Prelude.Int
startRemediationExecutionResponse_httpStatus = Lens.lens (\StartRemediationExecutionResponse' {httpStatus} -> httpStatus) (\s@StartRemediationExecutionResponse' {} a -> s {httpStatus = a} :: StartRemediationExecutionResponse)

instance
  Prelude.NFData
    StartRemediationExecutionResponse
  where
  rnf StartRemediationExecutionResponse' {..} =
    Prelude.rnf failedItems
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf httpStatus
