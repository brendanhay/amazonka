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
-- Module      : Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables the publishing of reputation metrics for emails sent
-- using a specific configuration set in a given AWS Region. Reputation
-- metrics include bounce and complaint rates. These metrics are published
-- to Amazon CloudWatch. By using CloudWatch, you can create alarms when
-- bounce or complaint rates exceed certain thresholds.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled
  ( -- * Creating a Request
    UpdateConfigurationSetReputationMetricsEnabled (..),
    newUpdateConfigurationSetReputationMetricsEnabled,

    -- * Request Lenses
    updateConfigurationSetReputationMetricsEnabled_configurationSetName,
    updateConfigurationSetReputationMetricsEnabled_enabled,

    -- * Destructuring the Response
    UpdateConfigurationSetReputationMetricsEnabledResponse (..),
    newUpdateConfigurationSetReputationMetricsEnabledResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to modify the reputation metric publishing settings
-- for a configuration set.
--
-- /See:/ 'newUpdateConfigurationSetReputationMetricsEnabled' smart constructor.
data UpdateConfigurationSetReputationMetricsEnabled = UpdateConfigurationSetReputationMetricsEnabled'
  { -- | The name of the configuration set that you want to update.
    configurationSetName :: Core.Text,
    -- | Describes whether or not Amazon SES will publish reputation metrics for
    -- the configuration set, such as bounce and complaint rates, to Amazon
    -- CloudWatch.
    enabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConfigurationSetReputationMetricsEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'updateConfigurationSetReputationMetricsEnabled_configurationSetName' - The name of the configuration set that you want to update.
--
-- 'enabled', 'updateConfigurationSetReputationMetricsEnabled_enabled' - Describes whether or not Amazon SES will publish reputation metrics for
-- the configuration set, such as bounce and complaint rates, to Amazon
-- CloudWatch.
newUpdateConfigurationSetReputationMetricsEnabled ::
  -- | 'configurationSetName'
  Core.Text ->
  -- | 'enabled'
  Core.Bool ->
  UpdateConfigurationSetReputationMetricsEnabled
newUpdateConfigurationSetReputationMetricsEnabled
  pConfigurationSetName_
  pEnabled_ =
    UpdateConfigurationSetReputationMetricsEnabled'
      { configurationSetName =
          pConfigurationSetName_,
        enabled = pEnabled_
      }

-- | The name of the configuration set that you want to update.
updateConfigurationSetReputationMetricsEnabled_configurationSetName :: Lens.Lens' UpdateConfigurationSetReputationMetricsEnabled Core.Text
updateConfigurationSetReputationMetricsEnabled_configurationSetName = Lens.lens (\UpdateConfigurationSetReputationMetricsEnabled' {configurationSetName} -> configurationSetName) (\s@UpdateConfigurationSetReputationMetricsEnabled' {} a -> s {configurationSetName = a} :: UpdateConfigurationSetReputationMetricsEnabled)

-- | Describes whether or not Amazon SES will publish reputation metrics for
-- the configuration set, such as bounce and complaint rates, to Amazon
-- CloudWatch.
updateConfigurationSetReputationMetricsEnabled_enabled :: Lens.Lens' UpdateConfigurationSetReputationMetricsEnabled Core.Bool
updateConfigurationSetReputationMetricsEnabled_enabled = Lens.lens (\UpdateConfigurationSetReputationMetricsEnabled' {enabled} -> enabled) (\s@UpdateConfigurationSetReputationMetricsEnabled' {} a -> s {enabled = a} :: UpdateConfigurationSetReputationMetricsEnabled)

instance
  Core.AWSRequest
    UpdateConfigurationSetReputationMetricsEnabled
  where
  type
    AWSResponse
      UpdateConfigurationSetReputationMetricsEnabled =
      UpdateConfigurationSetReputationMetricsEnabledResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UpdateConfigurationSetReputationMetricsEnabledResponse'

instance
  Core.Hashable
    UpdateConfigurationSetReputationMetricsEnabled

instance
  Core.NFData
    UpdateConfigurationSetReputationMetricsEnabled

instance
  Core.ToHeaders
    UpdateConfigurationSetReputationMetricsEnabled
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    UpdateConfigurationSetReputationMetricsEnabled
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateConfigurationSetReputationMetricsEnabled
  where
  toQuery
    UpdateConfigurationSetReputationMetricsEnabled' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "UpdateConfigurationSetReputationMetricsEnabled" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2010-12-01" :: Core.ByteString),
          "ConfigurationSetName" Core.=: configurationSetName,
          "Enabled" Core.=: enabled
        ]

-- | /See:/ 'newUpdateConfigurationSetReputationMetricsEnabledResponse' smart constructor.
data UpdateConfigurationSetReputationMetricsEnabledResponse = UpdateConfigurationSetReputationMetricsEnabledResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConfigurationSetReputationMetricsEnabledResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateConfigurationSetReputationMetricsEnabledResponse ::
  UpdateConfigurationSetReputationMetricsEnabledResponse
newUpdateConfigurationSetReputationMetricsEnabledResponse =
  UpdateConfigurationSetReputationMetricsEnabledResponse'

instance
  Core.NFData
    UpdateConfigurationSetReputationMetricsEnabledResponse
