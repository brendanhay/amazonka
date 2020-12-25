{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables the publishing of reputation metrics for emails sent using a specific configuration set in a given AWS Region. Reputation metrics include bounce and complaint rates. These metrics are published to Amazon CloudWatch. By using CloudWatch, you can create alarms when bounce or complaint rates exceed certain thresholds.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled
  ( -- * Creating a request
    UpdateConfigurationSetReputationMetricsEnabled (..),
    mkUpdateConfigurationSetReputationMetricsEnabled,

    -- ** Request lenses
    ucsrmeConfigurationSetName,
    ucsrmeEnabled,

    -- * Destructuring the response
    UpdateConfigurationSetReputationMetricsEnabledResponse (..),
    mkUpdateConfigurationSetReputationMetricsEnabledResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to modify the reputation metric publishing settings for a configuration set.
--
-- /See:/ 'mkUpdateConfigurationSetReputationMetricsEnabled' smart constructor.
data UpdateConfigurationSetReputationMetricsEnabled = UpdateConfigurationSetReputationMetricsEnabled'
  { -- | The name of the configuration set that you want to update.
    configurationSetName :: Types.ConfigurationSetName,
    -- | Describes whether or not Amazon SES will publish reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
    enabled :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConfigurationSetReputationMetricsEnabled' value with any optional fields omitted.
mkUpdateConfigurationSetReputationMetricsEnabled ::
  -- | 'configurationSetName'
  Types.ConfigurationSetName ->
  -- | 'enabled'
  Core.Bool ->
  UpdateConfigurationSetReputationMetricsEnabled
mkUpdateConfigurationSetReputationMetricsEnabled
  configurationSetName
  enabled =
    UpdateConfigurationSetReputationMetricsEnabled'
      { configurationSetName,
        enabled
      }

-- | The name of the configuration set that you want to update.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrmeConfigurationSetName :: Lens.Lens' UpdateConfigurationSetReputationMetricsEnabled Types.ConfigurationSetName
ucsrmeConfigurationSetName = Lens.field @"configurationSetName"
{-# DEPRECATED ucsrmeConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | Describes whether or not Amazon SES will publish reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrmeEnabled :: Lens.Lens' UpdateConfigurationSetReputationMetricsEnabled Core.Bool
ucsrmeEnabled = Lens.field @"enabled"
{-# DEPRECATED ucsrmeEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance
  Core.AWSRequest
    UpdateConfigurationSetReputationMetricsEnabled
  where
  type
    Rs UpdateConfigurationSetReputationMetricsEnabled =
      UpdateConfigurationSetReputationMetricsEnabledResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure
                ("Action", "UpdateConfigurationSetReputationMetricsEnabled")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ConfigurationSetName" configurationSetName)
                Core.<> (Core.toQueryValue "Enabled" enabled)
            )
      }
  response =
    Response.receiveNull
      UpdateConfigurationSetReputationMetricsEnabledResponse'

-- | /See:/ 'mkUpdateConfigurationSetReputationMetricsEnabledResponse' smart constructor.
data UpdateConfigurationSetReputationMetricsEnabledResponse = UpdateConfigurationSetReputationMetricsEnabledResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateConfigurationSetReputationMetricsEnabledResponse' value with any optional fields omitted.
mkUpdateConfigurationSetReputationMetricsEnabledResponse ::
  UpdateConfigurationSetReputationMetricsEnabledResponse
mkUpdateConfigurationSetReputationMetricsEnabledResponse =
  UpdateConfigurationSetReputationMetricsEnabledResponse'
