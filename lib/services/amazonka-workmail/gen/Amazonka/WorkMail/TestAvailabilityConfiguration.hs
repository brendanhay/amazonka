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
-- Module      : Amazonka.WorkMail.TestAvailabilityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs a test on an availability provider to ensure that access is
-- allowed. For EWS, it verifies the provided credentials can be used to
-- successfully log in. For Lambda, it verifies that the Lambda function
-- can be invoked and that the resource access policy was configured to
-- deny anonymous access. An anonymous invocation is one done without
-- providing either a @SourceArn@ or @SourceAccount@ header.
--
-- The request must contain either one provider definition (@EwsProvider@
-- or @LambdaProvider@) or the @DomainName@ parameter. If the @DomainName@
-- parameter is provided, the configuration stored under the @DomainName@
-- will be tested.
module Amazonka.WorkMail.TestAvailabilityConfiguration
  ( -- * Creating a Request
    TestAvailabilityConfiguration (..),
    newTestAvailabilityConfiguration,

    -- * Request Lenses
    testAvailabilityConfiguration_domainName,
    testAvailabilityConfiguration_ewsProvider,
    testAvailabilityConfiguration_lambdaProvider,
    testAvailabilityConfiguration_organizationId,

    -- * Destructuring the Response
    TestAvailabilityConfigurationResponse (..),
    newTestAvailabilityConfigurationResponse,

    -- * Response Lenses
    testAvailabilityConfigurationResponse_failureReason,
    testAvailabilityConfigurationResponse_testPassed,
    testAvailabilityConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newTestAvailabilityConfiguration' smart constructor.
data TestAvailabilityConfiguration = TestAvailabilityConfiguration'
  { -- | The domain to which the provider applies. If this field is provided, a
    -- stored availability provider associated to this domain name will be
    -- tested.
    domainName :: Prelude.Maybe Prelude.Text,
    ewsProvider :: Prelude.Maybe EwsAvailabilityProvider,
    lambdaProvider :: Prelude.Maybe LambdaAvailabilityProvider,
    -- | The WorkMail organization where the availability provider will be
    -- tested.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestAvailabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'testAvailabilityConfiguration_domainName' - The domain to which the provider applies. If this field is provided, a
-- stored availability provider associated to this domain name will be
-- tested.
--
-- 'ewsProvider', 'testAvailabilityConfiguration_ewsProvider' - Undocumented member.
--
-- 'lambdaProvider', 'testAvailabilityConfiguration_lambdaProvider' - Undocumented member.
--
-- 'organizationId', 'testAvailabilityConfiguration_organizationId' - The WorkMail organization where the availability provider will be
-- tested.
newTestAvailabilityConfiguration ::
  -- | 'organizationId'
  Prelude.Text ->
  TestAvailabilityConfiguration
newTestAvailabilityConfiguration pOrganizationId_ =
  TestAvailabilityConfiguration'
    { domainName =
        Prelude.Nothing,
      ewsProvider = Prelude.Nothing,
      lambdaProvider = Prelude.Nothing,
      organizationId = pOrganizationId_
    }

-- | The domain to which the provider applies. If this field is provided, a
-- stored availability provider associated to this domain name will be
-- tested.
testAvailabilityConfiguration_domainName :: Lens.Lens' TestAvailabilityConfiguration (Prelude.Maybe Prelude.Text)
testAvailabilityConfiguration_domainName = Lens.lens (\TestAvailabilityConfiguration' {domainName} -> domainName) (\s@TestAvailabilityConfiguration' {} a -> s {domainName = a} :: TestAvailabilityConfiguration)

-- | Undocumented member.
testAvailabilityConfiguration_ewsProvider :: Lens.Lens' TestAvailabilityConfiguration (Prelude.Maybe EwsAvailabilityProvider)
testAvailabilityConfiguration_ewsProvider = Lens.lens (\TestAvailabilityConfiguration' {ewsProvider} -> ewsProvider) (\s@TestAvailabilityConfiguration' {} a -> s {ewsProvider = a} :: TestAvailabilityConfiguration)

-- | Undocumented member.
testAvailabilityConfiguration_lambdaProvider :: Lens.Lens' TestAvailabilityConfiguration (Prelude.Maybe LambdaAvailabilityProvider)
testAvailabilityConfiguration_lambdaProvider = Lens.lens (\TestAvailabilityConfiguration' {lambdaProvider} -> lambdaProvider) (\s@TestAvailabilityConfiguration' {} a -> s {lambdaProvider = a} :: TestAvailabilityConfiguration)

-- | The WorkMail organization where the availability provider will be
-- tested.
testAvailabilityConfiguration_organizationId :: Lens.Lens' TestAvailabilityConfiguration Prelude.Text
testAvailabilityConfiguration_organizationId = Lens.lens (\TestAvailabilityConfiguration' {organizationId} -> organizationId) (\s@TestAvailabilityConfiguration' {} a -> s {organizationId = a} :: TestAvailabilityConfiguration)

instance
  Core.AWSRequest
    TestAvailabilityConfiguration
  where
  type
    AWSResponse TestAvailabilityConfiguration =
      TestAvailabilityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestAvailabilityConfigurationResponse'
            Prelude.<$> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "TestPassed")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    TestAvailabilityConfiguration
  where
  hashWithSalt _salt TestAvailabilityConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` ewsProvider
      `Prelude.hashWithSalt` lambdaProvider
      `Prelude.hashWithSalt` organizationId

instance Prelude.NFData TestAvailabilityConfiguration where
  rnf TestAvailabilityConfiguration' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf ewsProvider
      `Prelude.seq` Prelude.rnf lambdaProvider
      `Prelude.seq` Prelude.rnf organizationId

instance Data.ToHeaders TestAvailabilityConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.TestAvailabilityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TestAvailabilityConfiguration where
  toJSON TestAvailabilityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DomainName" Data..=) Prelude.<$> domainName,
            ("EwsProvider" Data..=) Prelude.<$> ewsProvider,
            ("LambdaProvider" Data..=)
              Prelude.<$> lambdaProvider,
            Prelude.Just
              ("OrganizationId" Data..= organizationId)
          ]
      )

instance Data.ToPath TestAvailabilityConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery TestAvailabilityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestAvailabilityConfigurationResponse' smart constructor.
data TestAvailabilityConfigurationResponse = TestAvailabilityConfigurationResponse'
  { -- | String containing the reason for a failed test if @TestPassed@ is false.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Boolean indicating whether the test passed or failed.
    testPassed :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestAvailabilityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'testAvailabilityConfigurationResponse_failureReason' - String containing the reason for a failed test if @TestPassed@ is false.
--
-- 'testPassed', 'testAvailabilityConfigurationResponse_testPassed' - Boolean indicating whether the test passed or failed.
--
-- 'httpStatus', 'testAvailabilityConfigurationResponse_httpStatus' - The response's http status code.
newTestAvailabilityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestAvailabilityConfigurationResponse
newTestAvailabilityConfigurationResponse pHttpStatus_ =
  TestAvailabilityConfigurationResponse'
    { failureReason =
        Prelude.Nothing,
      testPassed = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | String containing the reason for a failed test if @TestPassed@ is false.
testAvailabilityConfigurationResponse_failureReason :: Lens.Lens' TestAvailabilityConfigurationResponse (Prelude.Maybe Prelude.Text)
testAvailabilityConfigurationResponse_failureReason = Lens.lens (\TestAvailabilityConfigurationResponse' {failureReason} -> failureReason) (\s@TestAvailabilityConfigurationResponse' {} a -> s {failureReason = a} :: TestAvailabilityConfigurationResponse)

-- | Boolean indicating whether the test passed or failed.
testAvailabilityConfigurationResponse_testPassed :: Lens.Lens' TestAvailabilityConfigurationResponse (Prelude.Maybe Prelude.Bool)
testAvailabilityConfigurationResponse_testPassed = Lens.lens (\TestAvailabilityConfigurationResponse' {testPassed} -> testPassed) (\s@TestAvailabilityConfigurationResponse' {} a -> s {testPassed = a} :: TestAvailabilityConfigurationResponse)

-- | The response's http status code.
testAvailabilityConfigurationResponse_httpStatus :: Lens.Lens' TestAvailabilityConfigurationResponse Prelude.Int
testAvailabilityConfigurationResponse_httpStatus = Lens.lens (\TestAvailabilityConfigurationResponse' {httpStatus} -> httpStatus) (\s@TestAvailabilityConfigurationResponse' {} a -> s {httpStatus = a} :: TestAvailabilityConfigurationResponse)

instance
  Prelude.NFData
    TestAvailabilityConfigurationResponse
  where
  rnf TestAvailabilityConfigurationResponse' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf testPassed
      `Prelude.seq` Prelude.rnf httpStatus
