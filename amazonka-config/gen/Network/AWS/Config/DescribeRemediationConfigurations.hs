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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeRemediationConfigurations' smart constructor.
data DescribeRemediationConfigurations = DescribeRemediationConfigurations'
  { -- | A list of AWS Config rule names of remediation configurations for which
    -- you want details.
    configRuleNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.mempty
    }

-- | A list of AWS Config rule names of remediation configurations for which
-- you want details.
describeRemediationConfigurations_configRuleNames :: Lens.Lens' DescribeRemediationConfigurations [Core.Text]
describeRemediationConfigurations_configRuleNames = Lens.lens (\DescribeRemediationConfigurations' {configRuleNames} -> configRuleNames) (\s@DescribeRemediationConfigurations' {} a -> s {configRuleNames = a} :: DescribeRemediationConfigurations) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DescribeRemediationConfigurations
  where
  type
    AWSResponse DescribeRemediationConfigurations =
      DescribeRemediationConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRemediationConfigurationsResponse'
            Core.<$> ( x Core..?> "RemediationConfigurations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeRemediationConfigurations

instance
  Core.NFData
    DescribeRemediationConfigurations

instance
  Core.ToHeaders
    DescribeRemediationConfigurations
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeRemediationConfigurations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeRemediationConfigurations
  where
  toJSON DescribeRemediationConfigurations' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ConfigRuleNames" Core..= configRuleNames)
          ]
      )

instance
  Core.ToPath
    DescribeRemediationConfigurations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeRemediationConfigurations
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeRemediationConfigurationsResponse' smart constructor.
data DescribeRemediationConfigurationsResponse = DescribeRemediationConfigurationsResponse'
  { -- | Returns a remediation configuration object.
    remediationConfigurations :: Core.Maybe [RemediationConfiguration],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeRemediationConfigurationsResponse
newDescribeRemediationConfigurationsResponse
  pHttpStatus_ =
    DescribeRemediationConfigurationsResponse'
      { remediationConfigurations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a remediation configuration object.
describeRemediationConfigurationsResponse_remediationConfigurations :: Lens.Lens' DescribeRemediationConfigurationsResponse (Core.Maybe [RemediationConfiguration])
describeRemediationConfigurationsResponse_remediationConfigurations = Lens.lens (\DescribeRemediationConfigurationsResponse' {remediationConfigurations} -> remediationConfigurations) (\s@DescribeRemediationConfigurationsResponse' {} a -> s {remediationConfigurations = a} :: DescribeRemediationConfigurationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeRemediationConfigurationsResponse_httpStatus :: Lens.Lens' DescribeRemediationConfigurationsResponse Core.Int
describeRemediationConfigurationsResponse_httpStatus = Lens.lens (\DescribeRemediationConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeRemediationConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeRemediationConfigurationsResponse)

instance
  Core.NFData
    DescribeRemediationConfigurationsResponse
