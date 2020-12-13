{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateConfigurationSetSendingEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables email sending for messages sent using a specific configuration set in a given AWS Region. You can use this operation in conjunction with Amazon CloudWatch alarms to temporarily pause email sending for a configuration set when the reputation metrics for that configuration set (such as your bounce on complaint rate) exceed certain thresholds.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateConfigurationSetSendingEnabled
  ( -- * Creating a request
    UpdateConfigurationSetSendingEnabled (..),
    mkUpdateConfigurationSetSendingEnabled,

    -- ** Request lenses
    ucsseEnabled,
    ucsseConfigurationSetName,

    -- * Destructuring the response
    UpdateConfigurationSetSendingEnabledResponse (..),
    mkUpdateConfigurationSetSendingEnabledResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to enable or disable the email sending capabilities for a specific configuration set.
--
-- /See:/ 'mkUpdateConfigurationSetSendingEnabled' smart constructor.
data UpdateConfigurationSetSendingEnabled = UpdateConfigurationSetSendingEnabled'
  { -- | Describes whether email sending is enabled or disabled for the configuration set.
    enabled :: Lude.Bool,
    -- | The name of the configuration set that you want to update.
    configurationSetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfigurationSetSendingEnabled' with the minimum fields required to make a request.
--
-- * 'enabled' - Describes whether email sending is enabled or disabled for the configuration set.
-- * 'configurationSetName' - The name of the configuration set that you want to update.
mkUpdateConfigurationSetSendingEnabled ::
  -- | 'enabled'
  Lude.Bool ->
  -- | 'configurationSetName'
  Lude.Text ->
  UpdateConfigurationSetSendingEnabled
mkUpdateConfigurationSetSendingEnabled
  pEnabled_
  pConfigurationSetName_ =
    UpdateConfigurationSetSendingEnabled'
      { enabled = pEnabled_,
        configurationSetName = pConfigurationSetName_
      }

-- | Describes whether email sending is enabled or disabled for the configuration set.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsseEnabled :: Lens.Lens' UpdateConfigurationSetSendingEnabled Lude.Bool
ucsseEnabled = Lens.lens (enabled :: UpdateConfigurationSetSendingEnabled -> Lude.Bool) (\s a -> s {enabled = a} :: UpdateConfigurationSetSendingEnabled)
{-# DEPRECATED ucsseEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The name of the configuration set that you want to update.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsseConfigurationSetName :: Lens.Lens' UpdateConfigurationSetSendingEnabled Lude.Text
ucsseConfigurationSetName = Lens.lens (configurationSetName :: UpdateConfigurationSetSendingEnabled -> Lude.Text) (\s a -> s {configurationSetName = a} :: UpdateConfigurationSetSendingEnabled)
{-# DEPRECATED ucsseConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

instance Lude.AWSRequest UpdateConfigurationSetSendingEnabled where
  type
    Rs UpdateConfigurationSetSendingEnabled =
      UpdateConfigurationSetSendingEnabledResponse
  request = Req.postQuery sesService
  response =
    Res.receiveNull UpdateConfigurationSetSendingEnabledResponse'

instance Lude.ToHeaders UpdateConfigurationSetSendingEnabled where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateConfigurationSetSendingEnabled where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateConfigurationSetSendingEnabled where
  toQuery UpdateConfigurationSetSendingEnabled' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateConfigurationSetSendingEnabled" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Enabled" Lude.=: enabled,
        "ConfigurationSetName" Lude.=: configurationSetName
      ]

-- | /See:/ 'mkUpdateConfigurationSetSendingEnabledResponse' smart constructor.
data UpdateConfigurationSetSendingEnabledResponse = UpdateConfigurationSetSendingEnabledResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfigurationSetSendingEnabledResponse' with the minimum fields required to make a request.
mkUpdateConfigurationSetSendingEnabledResponse ::
  UpdateConfigurationSetSendingEnabledResponse
mkUpdateConfigurationSetSendingEnabledResponse =
  UpdateConfigurationSetSendingEnabledResponse'
