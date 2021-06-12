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
-- Module      : Network.AWS.Config.PutRemediationExceptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A remediation exception is when a specific resource is no longer
-- considered for auto-remediation. This API adds a new exception or
-- updates an existing exception for a specific resource with a specific
-- AWS Config rule.
--
-- AWS Config generates a remediation exception when a problem occurs
-- executing a remediation action to a specific resource. Remediation
-- exceptions blocks auto-remediation until the exception is cleared.
module Network.AWS.Config.PutRemediationExceptions
  ( -- * Creating a Request
    PutRemediationExceptions (..),
    newPutRemediationExceptions,

    -- * Request Lenses
    putRemediationExceptions_expirationTime,
    putRemediationExceptions_message,
    putRemediationExceptions_configRuleName,
    putRemediationExceptions_resourceKeys,

    -- * Destructuring the Response
    PutRemediationExceptionsResponse (..),
    newPutRemediationExceptionsResponse,

    -- * Response Lenses
    putRemediationExceptionsResponse_failedBatches,
    putRemediationExceptionsResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRemediationExceptions' smart constructor.
data PutRemediationExceptions = PutRemediationExceptions'
  { -- | The exception is automatically deleted after the expiration date.
    expirationTime :: Core.Maybe Core.POSIX,
    -- | The message contains an explanation of the exception.
    message :: Core.Maybe Core.Text,
    -- | The name of the AWS Config rule for which you want to create remediation
    -- exception.
    configRuleName :: Core.Text,
    -- | An exception list of resource exception keys to be processed with the
    -- current request. AWS Config adds exception for each resource key. For
    -- example, AWS Config adds 3 exceptions for 3 resource keys.
    resourceKeys :: Core.NonEmpty RemediationExceptionResourceKey
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRemediationExceptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationTime', 'putRemediationExceptions_expirationTime' - The exception is automatically deleted after the expiration date.
--
-- 'message', 'putRemediationExceptions_message' - The message contains an explanation of the exception.
--
-- 'configRuleName', 'putRemediationExceptions_configRuleName' - The name of the AWS Config rule for which you want to create remediation
-- exception.
--
-- 'resourceKeys', 'putRemediationExceptions_resourceKeys' - An exception list of resource exception keys to be processed with the
-- current request. AWS Config adds exception for each resource key. For
-- example, AWS Config adds 3 exceptions for 3 resource keys.
newPutRemediationExceptions ::
  -- | 'configRuleName'
  Core.Text ->
  -- | 'resourceKeys'
  Core.NonEmpty RemediationExceptionResourceKey ->
  PutRemediationExceptions
newPutRemediationExceptions
  pConfigRuleName_
  pResourceKeys_ =
    PutRemediationExceptions'
      { expirationTime =
          Core.Nothing,
        message = Core.Nothing,
        configRuleName = pConfigRuleName_,
        resourceKeys = Lens._Coerce Lens.# pResourceKeys_
      }

-- | The exception is automatically deleted after the expiration date.
putRemediationExceptions_expirationTime :: Lens.Lens' PutRemediationExceptions (Core.Maybe Core.UTCTime)
putRemediationExceptions_expirationTime = Lens.lens (\PutRemediationExceptions' {expirationTime} -> expirationTime) (\s@PutRemediationExceptions' {} a -> s {expirationTime = a} :: PutRemediationExceptions) Core.. Lens.mapping Core._Time

-- | The message contains an explanation of the exception.
putRemediationExceptions_message :: Lens.Lens' PutRemediationExceptions (Core.Maybe Core.Text)
putRemediationExceptions_message = Lens.lens (\PutRemediationExceptions' {message} -> message) (\s@PutRemediationExceptions' {} a -> s {message = a} :: PutRemediationExceptions)

-- | The name of the AWS Config rule for which you want to create remediation
-- exception.
putRemediationExceptions_configRuleName :: Lens.Lens' PutRemediationExceptions Core.Text
putRemediationExceptions_configRuleName = Lens.lens (\PutRemediationExceptions' {configRuleName} -> configRuleName) (\s@PutRemediationExceptions' {} a -> s {configRuleName = a} :: PutRemediationExceptions)

-- | An exception list of resource exception keys to be processed with the
-- current request. AWS Config adds exception for each resource key. For
-- example, AWS Config adds 3 exceptions for 3 resource keys.
putRemediationExceptions_resourceKeys :: Lens.Lens' PutRemediationExceptions (Core.NonEmpty RemediationExceptionResourceKey)
putRemediationExceptions_resourceKeys = Lens.lens (\PutRemediationExceptions' {resourceKeys} -> resourceKeys) (\s@PutRemediationExceptions' {} a -> s {resourceKeys = a} :: PutRemediationExceptions) Core.. Lens._Coerce

instance Core.AWSRequest PutRemediationExceptions where
  type
    AWSResponse PutRemediationExceptions =
      PutRemediationExceptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRemediationExceptionsResponse'
            Core.<$> (x Core..?> "FailedBatches" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutRemediationExceptions

instance Core.NFData PutRemediationExceptions

instance Core.ToHeaders PutRemediationExceptions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutRemediationExceptions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutRemediationExceptions where
  toJSON PutRemediationExceptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExpirationTime" Core..=) Core.<$> expirationTime,
            ("Message" Core..=) Core.<$> message,
            Core.Just ("ConfigRuleName" Core..= configRuleName),
            Core.Just ("ResourceKeys" Core..= resourceKeys)
          ]
      )

instance Core.ToPath PutRemediationExceptions where
  toPath = Core.const "/"

instance Core.ToQuery PutRemediationExceptions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutRemediationExceptionsResponse' smart constructor.
data PutRemediationExceptionsResponse = PutRemediationExceptionsResponse'
  { -- | Returns a list of failed remediation exceptions batch objects. Each
    -- object in the batch consists of a list of failed items and failure
    -- messages.
    failedBatches :: Core.Maybe [FailedRemediationExceptionBatch],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRemediationExceptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedBatches', 'putRemediationExceptionsResponse_failedBatches' - Returns a list of failed remediation exceptions batch objects. Each
-- object in the batch consists of a list of failed items and failure
-- messages.
--
-- 'httpStatus', 'putRemediationExceptionsResponse_httpStatus' - The response's http status code.
newPutRemediationExceptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutRemediationExceptionsResponse
newPutRemediationExceptionsResponse pHttpStatus_ =
  PutRemediationExceptionsResponse'
    { failedBatches =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of failed remediation exceptions batch objects. Each
-- object in the batch consists of a list of failed items and failure
-- messages.
putRemediationExceptionsResponse_failedBatches :: Lens.Lens' PutRemediationExceptionsResponse (Core.Maybe [FailedRemediationExceptionBatch])
putRemediationExceptionsResponse_failedBatches = Lens.lens (\PutRemediationExceptionsResponse' {failedBatches} -> failedBatches) (\s@PutRemediationExceptionsResponse' {} a -> s {failedBatches = a} :: PutRemediationExceptionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putRemediationExceptionsResponse_httpStatus :: Lens.Lens' PutRemediationExceptionsResponse Core.Int
putRemediationExceptionsResponse_httpStatus = Lens.lens (\PutRemediationExceptionsResponse' {httpStatus} -> httpStatus) (\s@PutRemediationExceptionsResponse' {} a -> s {httpStatus = a} :: PutRemediationExceptionsResponse)

instance Core.NFData PutRemediationExceptionsResponse
