{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.DescribeRemediationConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more remediation configurations.
module Network.AWS.Config.DescribeRemediationConfigurations
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

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRemediationConfigurations' smart constructor.
data DescribeRemediationConfigurations = DescribeRemediationConfigurations'
  { -- | A list of AWS Config rule names of remediation configurations for which
    -- you want details.
    configRuleNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeRemediationConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleNames', 'describeRemediationConfigurations_configRuleNames' - A list of AWS Config rule names of remediation configurations for which
-- you want details.
newDescribeRemediationConfigurations ::
  DescribeRemediationConfigurations
newDescribeRemediationConfigurations =
  DescribeRemediationConfigurations'
    { configRuleNames =
        Prelude.mempty
    }

-- | A list of AWS Config rule names of remediation configurations for which
-- you want details.
describeRemediationConfigurations_configRuleNames :: Lens.Lens' DescribeRemediationConfigurations [Prelude.Text]
describeRemediationConfigurations_configRuleNames = Lens.lens (\DescribeRemediationConfigurations' {configRuleNames} -> configRuleNames) (\s@DescribeRemediationConfigurations' {} a -> s {configRuleNames = a} :: DescribeRemediationConfigurations) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    DescribeRemediationConfigurations
  where
  type
    Rs DescribeRemediationConfigurations =
      DescribeRemediationConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRemediationConfigurationsResponse'
            Prelude.<$> ( x Prelude..?> "RemediationConfigurations"
                            Prelude..!@ Prelude.mempty
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeRemediationConfigurations

instance
  Prelude.NFData
    DescribeRemediationConfigurations

instance
  Prelude.ToHeaders
    DescribeRemediationConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DescribeRemediationConfigurations" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribeRemediationConfigurations
  where
  toJSON DescribeRemediationConfigurations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConfigRuleNames" Prelude..= configRuleNames)
          ]
      )

instance
  Prelude.ToPath
    DescribeRemediationConfigurations
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeRemediationConfigurationsResponse_remediationConfigurations = Lens.lens (\DescribeRemediationConfigurationsResponse' {remediationConfigurations} -> remediationConfigurations) (\s@DescribeRemediationConfigurationsResponse' {} a -> s {remediationConfigurations = a} :: DescribeRemediationConfigurationsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeRemediationConfigurationsResponse_httpStatus :: Lens.Lens' DescribeRemediationConfigurationsResponse Prelude.Int
describeRemediationConfigurationsResponse_httpStatus = Lens.lens (\DescribeRemediationConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeRemediationConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeRemediationConfigurationsResponse)

instance
  Prelude.NFData
    DescribeRemediationConfigurationsResponse
