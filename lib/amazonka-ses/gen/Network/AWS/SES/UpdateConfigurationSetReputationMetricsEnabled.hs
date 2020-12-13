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
    ucsrmeEnabled,
    ucsrmeConfigurationSetName,

    -- * Destructuring the response
    UpdateConfigurationSetReputationMetricsEnabledResponse (..),
    mkUpdateConfigurationSetReputationMetricsEnabledResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to modify the reputation metric publishing settings for a configuration set.
--
-- /See:/ 'mkUpdateConfigurationSetReputationMetricsEnabled' smart constructor.
data UpdateConfigurationSetReputationMetricsEnabled = UpdateConfigurationSetReputationMetricsEnabled'
  { -- | Describes whether or not Amazon SES will publish reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
    enabled :: Lude.Bool,
    -- | The name of the configuration set that you want to update.
    configurationSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfigurationSetReputationMetricsEnabled' with the minimum fields required to make a request.
--
-- * 'enabled' - Describes whether or not Amazon SES will publish reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
-- * 'configurationSetName' - The name of the configuration set that you want to update.
mkUpdateConfigurationSetReputationMetricsEnabled ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 'configurationSetName'
  Lude.Text ->
  UpdateConfigurationSetReputationMetricsEnabled
mkUpdateConfigurationSetReputationMetricsEnabled
  pEnabled_
  pConfigurationSetName_ =
    UpdateConfigurationSetReputationMetricsEnabled'
      { enabled =
          pEnabled_,
        configurationSetName = pConfigurationSetName_
      }

-- | Describes whether or not Amazon SES will publish reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrmeEnabled :: Lens.Lens' UpdateConfigurationSetReputationMetricsEnabled Lude.Bool
ucsrmeEnabled = Lens.lens (enabled :: UpdateConfigurationSetReputationMetricsEnabled -> Lude.Bool) (\s a -> s {enabled = a} :: UpdateConfigurationSetReputationMetricsEnabled)
{-# DEPRECATED ucsrmeEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The name of the configuration set that you want to update.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrmeConfigurationSetName :: Lens.Lens' UpdateConfigurationSetReputationMetricsEnabled Lude.Text
ucsrmeConfigurationSetName = Lens.lens (configurationSetName :: UpdateConfigurationSetReputationMetricsEnabled -> Lude.Text) (\s a -> s {configurationSetName = a} :: UpdateConfigurationSetReputationMetricsEnabled)
{-# DEPRECATED ucsrmeConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

instance
  Lude.AWSRequest
    UpdateConfigurationSetReputationMetricsEnabled
  where
  type
    Rs UpdateConfigurationSetReputationMetricsEnabled =
      UpdateConfigurationSetReputationMetricsEnabledResponse
  request = Req.postQuery sesService
  response =
    Res.receiveNull
      UpdateConfigurationSetReputationMetricsEnabledResponse'

instance
  Lude.ToHeaders
    UpdateConfigurationSetReputationMetricsEnabled
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateConfigurationSetReputationMetricsEnabled where
  toPath = Lude.const "/"

instance
  Lude.ToQuery
    UpdateConfigurationSetReputationMetricsEnabled
  where
  toQuery UpdateConfigurationSetReputationMetricsEnabled' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ( "UpdateConfigurationSetReputationMetricsEnabled" ::
                      Lude.ByteString
                  ),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Enabled" Lude.=: enabled,
        "ConfigurationSetName" Lude.=: configurationSetName
      ]

-- | /See:/ 'mkUpdateConfigurationSetReputationMetricsEnabledResponse' smart constructor.
data UpdateConfigurationSetReputationMetricsEnabledResponse = UpdateConfigurationSetReputationMetricsEnabledResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfigurationSetReputationMetricsEnabledResponse' with the minimum fields required to make a request.
mkUpdateConfigurationSetReputationMetricsEnabledResponse ::
  UpdateConfigurationSetReputationMetricsEnabledResponse
mkUpdateConfigurationSetReputationMetricsEnabledResponse =
  UpdateConfigurationSetReputationMetricsEnabledResponse'
