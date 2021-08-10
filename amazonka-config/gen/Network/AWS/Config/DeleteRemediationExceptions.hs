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
-- Module      : Network.AWS.Config.DeleteRemediationExceptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more remediation exceptions mentioned in the resource
-- keys.
--
-- AWS Config generates a remediation exception when a problem occurs
-- executing a remediation action to a specific resource. Remediation
-- exceptions blocks auto-remediation until the exception is cleared.
module Network.AWS.Config.DeleteRemediationExceptions
  ( -- * Creating a Request
    DeleteRemediationExceptions (..),
    newDeleteRemediationExceptions,

    -- * Request Lenses
    deleteRemediationExceptions_configRuleName,
    deleteRemediationExceptions_resourceKeys,

    -- * Destructuring the Response
    DeleteRemediationExceptionsResponse (..),
    newDeleteRemediationExceptionsResponse,

    -- * Response Lenses
    deleteRemediationExceptionsResponse_failedBatches,
    deleteRemediationExceptionsResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRemediationExceptions' smart constructor.
data DeleteRemediationExceptions = DeleteRemediationExceptions'
  { -- | The name of the AWS Config rule for which you want to delete remediation
    -- exception configuration.
    configRuleName :: Prelude.Text,
    -- | An exception list of resource exception keys to be processed with the
    -- current request. AWS Config adds exception for each resource key. For
    -- example, AWS Config adds 3 exceptions for 3 resource keys.
    resourceKeys :: Prelude.NonEmpty RemediationExceptionResourceKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRemediationExceptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'deleteRemediationExceptions_configRuleName' - The name of the AWS Config rule for which you want to delete remediation
-- exception configuration.
--
-- 'resourceKeys', 'deleteRemediationExceptions_resourceKeys' - An exception list of resource exception keys to be processed with the
-- current request. AWS Config adds exception for each resource key. For
-- example, AWS Config adds 3 exceptions for 3 resource keys.
newDeleteRemediationExceptions ::
  -- | 'configRuleName'
  Prelude.Text ->
  -- | 'resourceKeys'
  Prelude.NonEmpty RemediationExceptionResourceKey ->
  DeleteRemediationExceptions
newDeleteRemediationExceptions
  pConfigRuleName_
  pResourceKeys_ =
    DeleteRemediationExceptions'
      { configRuleName =
          pConfigRuleName_,
        resourceKeys =
          Lens._Coerce Lens.# pResourceKeys_
      }

-- | The name of the AWS Config rule for which you want to delete remediation
-- exception configuration.
deleteRemediationExceptions_configRuleName :: Lens.Lens' DeleteRemediationExceptions Prelude.Text
deleteRemediationExceptions_configRuleName = Lens.lens (\DeleteRemediationExceptions' {configRuleName} -> configRuleName) (\s@DeleteRemediationExceptions' {} a -> s {configRuleName = a} :: DeleteRemediationExceptions)

-- | An exception list of resource exception keys to be processed with the
-- current request. AWS Config adds exception for each resource key. For
-- example, AWS Config adds 3 exceptions for 3 resource keys.
deleteRemediationExceptions_resourceKeys :: Lens.Lens' DeleteRemediationExceptions (Prelude.NonEmpty RemediationExceptionResourceKey)
deleteRemediationExceptions_resourceKeys = Lens.lens (\DeleteRemediationExceptions' {resourceKeys} -> resourceKeys) (\s@DeleteRemediationExceptions' {} a -> s {resourceKeys = a} :: DeleteRemediationExceptions) Prelude.. Lens._Coerce

instance Core.AWSRequest DeleteRemediationExceptions where
  type
    AWSResponse DeleteRemediationExceptions =
      DeleteRemediationExceptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRemediationExceptionsResponse'
            Prelude.<$> (x Core..?> "FailedBatches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRemediationExceptions

instance Prelude.NFData DeleteRemediationExceptions

instance Core.ToHeaders DeleteRemediationExceptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteRemediationExceptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteRemediationExceptions where
  toJSON DeleteRemediationExceptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConfigRuleName" Core..= configRuleName),
            Prelude.Just ("ResourceKeys" Core..= resourceKeys)
          ]
      )

instance Core.ToPath DeleteRemediationExceptions where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteRemediationExceptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRemediationExceptionsResponse' smart constructor.
data DeleteRemediationExceptionsResponse = DeleteRemediationExceptionsResponse'
  { -- | Returns a list of failed delete remediation exceptions batch objects.
    -- Each object in the batch consists of a list of failed items and failure
    -- messages.
    failedBatches :: Prelude.Maybe [FailedDeleteRemediationExceptionsBatch],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRemediationExceptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedBatches', 'deleteRemediationExceptionsResponse_failedBatches' - Returns a list of failed delete remediation exceptions batch objects.
-- Each object in the batch consists of a list of failed items and failure
-- messages.
--
-- 'httpStatus', 'deleteRemediationExceptionsResponse_httpStatus' - The response's http status code.
newDeleteRemediationExceptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRemediationExceptionsResponse
newDeleteRemediationExceptionsResponse pHttpStatus_ =
  DeleteRemediationExceptionsResponse'
    { failedBatches =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of failed delete remediation exceptions batch objects.
-- Each object in the batch consists of a list of failed items and failure
-- messages.
deleteRemediationExceptionsResponse_failedBatches :: Lens.Lens' DeleteRemediationExceptionsResponse (Prelude.Maybe [FailedDeleteRemediationExceptionsBatch])
deleteRemediationExceptionsResponse_failedBatches = Lens.lens (\DeleteRemediationExceptionsResponse' {failedBatches} -> failedBatches) (\s@DeleteRemediationExceptionsResponse' {} a -> s {failedBatches = a} :: DeleteRemediationExceptionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteRemediationExceptionsResponse_httpStatus :: Lens.Lens' DeleteRemediationExceptionsResponse Prelude.Int
deleteRemediationExceptionsResponse_httpStatus = Lens.lens (\DeleteRemediationExceptionsResponse' {httpStatus} -> httpStatus) (\s@DeleteRemediationExceptionsResponse' {} a -> s {httpStatus = a} :: DeleteRemediationExceptionsResponse)

instance
  Prelude.NFData
    DeleteRemediationExceptionsResponse
