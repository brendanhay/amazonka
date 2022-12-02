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
-- Module      : Amazonka.Config.DescribeRemediationExceptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more remediation exceptions. A detailed
-- view of a remediation exception for a set of resources that includes an
-- explanation of an exception and the time when the exception will be
-- deleted. When you specify the limit and the next token, you receive a
-- paginated response.
--
-- Config generates a remediation exception when a problem occurs executing
-- a remediation action to a specific resource. Remediation exceptions
-- blocks auto-remediation until the exception is cleared.
--
-- When you specify the limit and the next token, you receive a paginated
-- response.
--
-- Limit and next token are not applicable if you request resources in
-- batch. It is only applicable, when you request all resources.
module Amazonka.Config.DescribeRemediationExceptions
  ( -- * Creating a Request
    DescribeRemediationExceptions (..),
    newDescribeRemediationExceptions,

    -- * Request Lenses
    describeRemediationExceptions_nextToken,
    describeRemediationExceptions_resourceKeys,
    describeRemediationExceptions_limit,
    describeRemediationExceptions_configRuleName,

    -- * Destructuring the Response
    DescribeRemediationExceptionsResponse (..),
    newDescribeRemediationExceptionsResponse,

    -- * Response Lenses
    describeRemediationExceptionsResponse_nextToken,
    describeRemediationExceptionsResponse_remediationExceptions,
    describeRemediationExceptionsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRemediationExceptions' smart constructor.
data DescribeRemediationExceptions = DescribeRemediationExceptions'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An exception list of resource exception keys to be processed with the
    -- current request. Config adds exception for each resource key. For
    -- example, Config adds 3 exceptions for 3 resource keys.
    resourceKeys :: Prelude.Maybe (Prelude.NonEmpty RemediationExceptionResourceKey),
    -- | The maximum number of RemediationExceptionResourceKey returned on each
    -- page. The default is 25. If you specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Config rule.
    configRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRemediationExceptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRemediationExceptions_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'resourceKeys', 'describeRemediationExceptions_resourceKeys' - An exception list of resource exception keys to be processed with the
-- current request. Config adds exception for each resource key. For
-- example, Config adds 3 exceptions for 3 resource keys.
--
-- 'limit', 'describeRemediationExceptions_limit' - The maximum number of RemediationExceptionResourceKey returned on each
-- page. The default is 25. If you specify 0, Config uses the default.
--
-- 'configRuleName', 'describeRemediationExceptions_configRuleName' - The name of the Config rule.
newDescribeRemediationExceptions ::
  -- | 'configRuleName'
  Prelude.Text ->
  DescribeRemediationExceptions
newDescribeRemediationExceptions pConfigRuleName_ =
  DescribeRemediationExceptions'
    { nextToken =
        Prelude.Nothing,
      resourceKeys = Prelude.Nothing,
      limit = Prelude.Nothing,
      configRuleName = pConfigRuleName_
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeRemediationExceptions_nextToken :: Lens.Lens' DescribeRemediationExceptions (Prelude.Maybe Prelude.Text)
describeRemediationExceptions_nextToken = Lens.lens (\DescribeRemediationExceptions' {nextToken} -> nextToken) (\s@DescribeRemediationExceptions' {} a -> s {nextToken = a} :: DescribeRemediationExceptions)

-- | An exception list of resource exception keys to be processed with the
-- current request. Config adds exception for each resource key. For
-- example, Config adds 3 exceptions for 3 resource keys.
describeRemediationExceptions_resourceKeys :: Lens.Lens' DescribeRemediationExceptions (Prelude.Maybe (Prelude.NonEmpty RemediationExceptionResourceKey))
describeRemediationExceptions_resourceKeys = Lens.lens (\DescribeRemediationExceptions' {resourceKeys} -> resourceKeys) (\s@DescribeRemediationExceptions' {} a -> s {resourceKeys = a} :: DescribeRemediationExceptions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of RemediationExceptionResourceKey returned on each
-- page. The default is 25. If you specify 0, Config uses the default.
describeRemediationExceptions_limit :: Lens.Lens' DescribeRemediationExceptions (Prelude.Maybe Prelude.Natural)
describeRemediationExceptions_limit = Lens.lens (\DescribeRemediationExceptions' {limit} -> limit) (\s@DescribeRemediationExceptions' {} a -> s {limit = a} :: DescribeRemediationExceptions)

-- | The name of the Config rule.
describeRemediationExceptions_configRuleName :: Lens.Lens' DescribeRemediationExceptions Prelude.Text
describeRemediationExceptions_configRuleName = Lens.lens (\DescribeRemediationExceptions' {configRuleName} -> configRuleName) (\s@DescribeRemediationExceptions' {} a -> s {configRuleName = a} :: DescribeRemediationExceptions)

instance
  Core.AWSRequest
    DescribeRemediationExceptions
  where
  type
    AWSResponse DescribeRemediationExceptions =
      DescribeRemediationExceptionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRemediationExceptionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "RemediationExceptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRemediationExceptions
  where
  hashWithSalt _salt DescribeRemediationExceptions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceKeys
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` configRuleName

instance Prelude.NFData DescribeRemediationExceptions where
  rnf DescribeRemediationExceptions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceKeys
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf configRuleName

instance Data.ToHeaders DescribeRemediationExceptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeRemediationExceptions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRemediationExceptions where
  toJSON DescribeRemediationExceptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceKeys" Data..=) Prelude.<$> resourceKeys,
            ("Limit" Data..=) Prelude.<$> limit,
            Prelude.Just
              ("ConfigRuleName" Data..= configRuleName)
          ]
      )

instance Data.ToPath DescribeRemediationExceptions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRemediationExceptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRemediationExceptionsResponse' smart constructor.
data DescribeRemediationExceptionsResponse = DescribeRemediationExceptionsResponse'
  { -- | The @nextToken@ string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of remediation exception objects.
    remediationExceptions :: Prelude.Maybe [RemediationException],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRemediationExceptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRemediationExceptionsResponse_nextToken' - The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'remediationExceptions', 'describeRemediationExceptionsResponse_remediationExceptions' - Returns a list of remediation exception objects.
--
-- 'httpStatus', 'describeRemediationExceptionsResponse_httpStatus' - The response's http status code.
newDescribeRemediationExceptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRemediationExceptionsResponse
newDescribeRemediationExceptionsResponse pHttpStatus_ =
  DescribeRemediationExceptionsResponse'
    { nextToken =
        Prelude.Nothing,
      remediationExceptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ string returned in a previous request that you use to
-- request the next page of results in a paginated response.
describeRemediationExceptionsResponse_nextToken :: Lens.Lens' DescribeRemediationExceptionsResponse (Prelude.Maybe Prelude.Text)
describeRemediationExceptionsResponse_nextToken = Lens.lens (\DescribeRemediationExceptionsResponse' {nextToken} -> nextToken) (\s@DescribeRemediationExceptionsResponse' {} a -> s {nextToken = a} :: DescribeRemediationExceptionsResponse)

-- | Returns a list of remediation exception objects.
describeRemediationExceptionsResponse_remediationExceptions :: Lens.Lens' DescribeRemediationExceptionsResponse (Prelude.Maybe [RemediationException])
describeRemediationExceptionsResponse_remediationExceptions = Lens.lens (\DescribeRemediationExceptionsResponse' {remediationExceptions} -> remediationExceptions) (\s@DescribeRemediationExceptionsResponse' {} a -> s {remediationExceptions = a} :: DescribeRemediationExceptionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRemediationExceptionsResponse_httpStatus :: Lens.Lens' DescribeRemediationExceptionsResponse Prelude.Int
describeRemediationExceptionsResponse_httpStatus = Lens.lens (\DescribeRemediationExceptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeRemediationExceptionsResponse' {} a -> s {httpStatus = a} :: DescribeRemediationExceptionsResponse)

instance
  Prelude.NFData
    DescribeRemediationExceptionsResponse
  where
  rnf DescribeRemediationExceptionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf remediationExceptions
      `Prelude.seq` Prelude.rnf httpStatus
