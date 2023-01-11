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
-- Module      : Amazonka.Config.PutRemediationExceptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A remediation exception is when a specific resource is no longer
-- considered for auto-remediation. This API adds a new exception or
-- updates an existing exception for a specific resource with a specific
-- Config rule.
--
-- Config generates a remediation exception when a problem occurs executing
-- a remediation action to a specific resource. Remediation exceptions
-- blocks auto-remediation until the exception is cleared.
--
-- To place an exception on an Amazon Web Services resource, ensure
-- remediation is set as manual remediation.
module Amazonka.Config.PutRemediationExceptions
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRemediationExceptions' smart constructor.
data PutRemediationExceptions = PutRemediationExceptions'
  { -- | The exception is automatically deleted after the expiration date.
    expirationTime :: Prelude.Maybe Data.POSIX,
    -- | The message contains an explanation of the exception.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the Config rule for which you want to create remediation
    -- exception.
    configRuleName :: Prelude.Text,
    -- | An exception list of resource exception keys to be processed with the
    -- current request. Config adds exception for each resource key. For
    -- example, Config adds 3 exceptions for 3 resource keys.
    resourceKeys :: Prelude.NonEmpty RemediationExceptionResourceKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'configRuleName', 'putRemediationExceptions_configRuleName' - The name of the Config rule for which you want to create remediation
-- exception.
--
-- 'resourceKeys', 'putRemediationExceptions_resourceKeys' - An exception list of resource exception keys to be processed with the
-- current request. Config adds exception for each resource key. For
-- example, Config adds 3 exceptions for 3 resource keys.
newPutRemediationExceptions ::
  -- | 'configRuleName'
  Prelude.Text ->
  -- | 'resourceKeys'
  Prelude.NonEmpty RemediationExceptionResourceKey ->
  PutRemediationExceptions
newPutRemediationExceptions
  pConfigRuleName_
  pResourceKeys_ =
    PutRemediationExceptions'
      { expirationTime =
          Prelude.Nothing,
        message = Prelude.Nothing,
        configRuleName = pConfigRuleName_,
        resourceKeys = Lens.coerced Lens.# pResourceKeys_
      }

-- | The exception is automatically deleted after the expiration date.
putRemediationExceptions_expirationTime :: Lens.Lens' PutRemediationExceptions (Prelude.Maybe Prelude.UTCTime)
putRemediationExceptions_expirationTime = Lens.lens (\PutRemediationExceptions' {expirationTime} -> expirationTime) (\s@PutRemediationExceptions' {} a -> s {expirationTime = a} :: PutRemediationExceptions) Prelude.. Lens.mapping Data._Time

-- | The message contains an explanation of the exception.
putRemediationExceptions_message :: Lens.Lens' PutRemediationExceptions (Prelude.Maybe Prelude.Text)
putRemediationExceptions_message = Lens.lens (\PutRemediationExceptions' {message} -> message) (\s@PutRemediationExceptions' {} a -> s {message = a} :: PutRemediationExceptions)

-- | The name of the Config rule for which you want to create remediation
-- exception.
putRemediationExceptions_configRuleName :: Lens.Lens' PutRemediationExceptions Prelude.Text
putRemediationExceptions_configRuleName = Lens.lens (\PutRemediationExceptions' {configRuleName} -> configRuleName) (\s@PutRemediationExceptions' {} a -> s {configRuleName = a} :: PutRemediationExceptions)

-- | An exception list of resource exception keys to be processed with the
-- current request. Config adds exception for each resource key. For
-- example, Config adds 3 exceptions for 3 resource keys.
putRemediationExceptions_resourceKeys :: Lens.Lens' PutRemediationExceptions (Prelude.NonEmpty RemediationExceptionResourceKey)
putRemediationExceptions_resourceKeys = Lens.lens (\PutRemediationExceptions' {resourceKeys} -> resourceKeys) (\s@PutRemediationExceptions' {} a -> s {resourceKeys = a} :: PutRemediationExceptions) Prelude.. Lens.coerced

instance Core.AWSRequest PutRemediationExceptions where
  type
    AWSResponse PutRemediationExceptions =
      PutRemediationExceptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRemediationExceptionsResponse'
            Prelude.<$> (x Data..?> "FailedBatches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRemediationExceptions where
  hashWithSalt _salt PutRemediationExceptions' {..} =
    _salt `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` configRuleName
      `Prelude.hashWithSalt` resourceKeys

instance Prelude.NFData PutRemediationExceptions where
  rnf PutRemediationExceptions' {..} =
    Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf configRuleName
      `Prelude.seq` Prelude.rnf resourceKeys

instance Data.ToHeaders PutRemediationExceptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.PutRemediationExceptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRemediationExceptions where
  toJSON PutRemediationExceptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExpirationTime" Data..=)
              Prelude.<$> expirationTime,
            ("Message" Data..=) Prelude.<$> message,
            Prelude.Just
              ("ConfigRuleName" Data..= configRuleName),
            Prelude.Just ("ResourceKeys" Data..= resourceKeys)
          ]
      )

instance Data.ToPath PutRemediationExceptions where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRemediationExceptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRemediationExceptionsResponse' smart constructor.
data PutRemediationExceptionsResponse = PutRemediationExceptionsResponse'
  { -- | Returns a list of failed remediation exceptions batch objects. Each
    -- object in the batch consists of a list of failed items and failure
    -- messages.
    failedBatches :: Prelude.Maybe [FailedRemediationExceptionBatch],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutRemediationExceptionsResponse
newPutRemediationExceptionsResponse pHttpStatus_ =
  PutRemediationExceptionsResponse'
    { failedBatches =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of failed remediation exceptions batch objects. Each
-- object in the batch consists of a list of failed items and failure
-- messages.
putRemediationExceptionsResponse_failedBatches :: Lens.Lens' PutRemediationExceptionsResponse (Prelude.Maybe [FailedRemediationExceptionBatch])
putRemediationExceptionsResponse_failedBatches = Lens.lens (\PutRemediationExceptionsResponse' {failedBatches} -> failedBatches) (\s@PutRemediationExceptionsResponse' {} a -> s {failedBatches = a} :: PutRemediationExceptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putRemediationExceptionsResponse_httpStatus :: Lens.Lens' PutRemediationExceptionsResponse Prelude.Int
putRemediationExceptionsResponse_httpStatus = Lens.lens (\PutRemediationExceptionsResponse' {httpStatus} -> httpStatus) (\s@PutRemediationExceptionsResponse' {} a -> s {httpStatus = a} :: PutRemediationExceptionsResponse)

instance
  Prelude.NFData
    PutRemediationExceptionsResponse
  where
  rnf PutRemediationExceptionsResponse' {..} =
    Prelude.rnf failedBatches
      `Prelude.seq` Prelude.rnf httpStatus
