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
-- Module      : Amazonka.Config.DescribeRemediationConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more remediation configurations.
module Amazonka.Config.DescribeRemediationConfigurations
  ( -- * Creating a Request
    DescribeRemediationConfigurations (..),
    newDescribeRemediationConfigurations,

    -- * Request Lenses
    describeRemediationConfigurations_configRuleNames,

    -- * Destructuring the Response
    DescribeRemediationConfigurationsResponse (..),
    newDescribeRemediationConfigurationsResponse,

    -- * Response Lenses
    describeRemediationConfigurationsResponse_remediationConfigurations,
    describeRemediationConfigurationsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRemediationConfigurations' smart constructor.
data DescribeRemediationConfigurations = DescribeRemediationConfigurations'
  { -- | A list of Config rule names of remediation configurations for which you
    -- want details.
    configRuleNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRemediationConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleNames', 'describeRemediationConfigurations_configRuleNames' - A list of Config rule names of remediation configurations for which you
-- want details.
newDescribeRemediationConfigurations ::
  DescribeRemediationConfigurations
newDescribeRemediationConfigurations =
  DescribeRemediationConfigurations'
    { configRuleNames =
        Prelude.mempty
    }

-- | A list of Config rule names of remediation configurations for which you
-- want details.
describeRemediationConfigurations_configRuleNames :: Lens.Lens' DescribeRemediationConfigurations [Prelude.Text]
describeRemediationConfigurations_configRuleNames = Lens.lens (\DescribeRemediationConfigurations' {configRuleNames} -> configRuleNames) (\s@DescribeRemediationConfigurations' {} a -> s {configRuleNames = a} :: DescribeRemediationConfigurations) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DescribeRemediationConfigurations
  where
  type
    AWSResponse DescribeRemediationConfigurations =
      DescribeRemediationConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRemediationConfigurationsResponse'
            Prelude.<$> ( x
                            Data..?> "RemediationConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRemediationConfigurations
  where
  hashWithSalt
    _salt
    DescribeRemediationConfigurations' {..} =
      _salt `Prelude.hashWithSalt` configRuleNames

instance
  Prelude.NFData
    DescribeRemediationConfigurations
  where
  rnf DescribeRemediationConfigurations' {..} =
    Prelude.rnf configRuleNames

instance
  Data.ToHeaders
    DescribeRemediationConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeRemediationConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeRemediationConfigurations
  where
  toJSON DescribeRemediationConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConfigRuleNames" Data..= configRuleNames)
          ]
      )

instance
  Data.ToPath
    DescribeRemediationConfigurations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeRemediationConfigurations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRemediationConfigurationsResponse' smart constructor.
data DescribeRemediationConfigurationsResponse = DescribeRemediationConfigurationsResponse'
  { -- | Returns a remediation configuration object.
    remediationConfigurations :: Prelude.Maybe [RemediationConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRemediationConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remediationConfigurations', 'describeRemediationConfigurationsResponse_remediationConfigurations' - Returns a remediation configuration object.
--
-- 'httpStatus', 'describeRemediationConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeRemediationConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRemediationConfigurationsResponse
newDescribeRemediationConfigurationsResponse
  pHttpStatus_ =
    DescribeRemediationConfigurationsResponse'
      { remediationConfigurations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a remediation configuration object.
describeRemediationConfigurationsResponse_remediationConfigurations :: Lens.Lens' DescribeRemediationConfigurationsResponse (Prelude.Maybe [RemediationConfiguration])
describeRemediationConfigurationsResponse_remediationConfigurations = Lens.lens (\DescribeRemediationConfigurationsResponse' {remediationConfigurations} -> remediationConfigurations) (\s@DescribeRemediationConfigurationsResponse' {} a -> s {remediationConfigurations = a} :: DescribeRemediationConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRemediationConfigurationsResponse_httpStatus :: Lens.Lens' DescribeRemediationConfigurationsResponse Prelude.Int
describeRemediationConfigurationsResponse_httpStatus = Lens.lens (\DescribeRemediationConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeRemediationConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeRemediationConfigurationsResponse)

instance
  Prelude.NFData
    DescribeRemediationConfigurationsResponse
  where
  rnf DescribeRemediationConfigurationsResponse' {..} =
    Prelude.rnf remediationConfigurations `Prelude.seq`
      Prelude.rnf httpStatus
