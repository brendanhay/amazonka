{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.UpdateConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an association between a configuration set and a custom domain for open and click event tracking.
--
-- By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using custom domains, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Amazon SES Developer Guide> .
module Network.AWS.SES.UpdateConfigurationSetTrackingOptions
  ( -- * Creating a request
    UpdateConfigurationSetTrackingOptions (..),
    mkUpdateConfigurationSetTrackingOptions,

    -- ** Request lenses
    ucstoConfigurationSetName,
    ucstoTrackingOptions,

    -- * Destructuring the response
    UpdateConfigurationSetTrackingOptionsResponse (..),
    mkUpdateConfigurationSetTrackingOptionsResponse,

    -- ** Response lenses
    ucstorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to update the tracking options for a configuration set.
--
-- /See:/ 'mkUpdateConfigurationSetTrackingOptions' smart constructor.
data UpdateConfigurationSetTrackingOptions = UpdateConfigurationSetTrackingOptions'
  { -- | The name of the configuration set for which you want to update the custom tracking domain.
    configurationSetName :: Lude.Text,
    trackingOptions :: TrackingOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfigurationSetTrackingOptions' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - The name of the configuration set for which you want to update the custom tracking domain.
-- * 'trackingOptions' -
mkUpdateConfigurationSetTrackingOptions ::
  -- | 'configurationSetName'
  Lude.Text ->
  -- | 'trackingOptions'
  TrackingOptions ->
  UpdateConfigurationSetTrackingOptions
mkUpdateConfigurationSetTrackingOptions
  pConfigurationSetName_
  pTrackingOptions_ =
    UpdateConfigurationSetTrackingOptions'
      { configurationSetName =
          pConfigurationSetName_,
        trackingOptions = pTrackingOptions_
      }

-- | The name of the configuration set for which you want to update the custom tracking domain.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucstoConfigurationSetName :: Lens.Lens' UpdateConfigurationSetTrackingOptions Lude.Text
ucstoConfigurationSetName = Lens.lens (configurationSetName :: UpdateConfigurationSetTrackingOptions -> Lude.Text) (\s a -> s {configurationSetName = a} :: UpdateConfigurationSetTrackingOptions)
{-# DEPRECATED ucstoConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trackingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucstoTrackingOptions :: Lens.Lens' UpdateConfigurationSetTrackingOptions TrackingOptions
ucstoTrackingOptions = Lens.lens (trackingOptions :: UpdateConfigurationSetTrackingOptions -> TrackingOptions) (\s a -> s {trackingOptions = a} :: UpdateConfigurationSetTrackingOptions)
{-# DEPRECATED ucstoTrackingOptions "Use generic-lens or generic-optics with 'trackingOptions' instead." #-}

instance Lude.AWSRequest UpdateConfigurationSetTrackingOptions where
  type
    Rs UpdateConfigurationSetTrackingOptions =
      UpdateConfigurationSetTrackingOptionsResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "UpdateConfigurationSetTrackingOptionsResult"
      ( \s h x ->
          UpdateConfigurationSetTrackingOptionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConfigurationSetTrackingOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateConfigurationSetTrackingOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateConfigurationSetTrackingOptions where
  toQuery UpdateConfigurationSetTrackingOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateConfigurationSetTrackingOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSetName" Lude.=: configurationSetName,
        "TrackingOptions" Lude.=: trackingOptions
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkUpdateConfigurationSetTrackingOptionsResponse' smart constructor.
newtype UpdateConfigurationSetTrackingOptionsResponse = UpdateConfigurationSetTrackingOptionsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConfigurationSetTrackingOptionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateConfigurationSetTrackingOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConfigurationSetTrackingOptionsResponse
mkUpdateConfigurationSetTrackingOptionsResponse pResponseStatus_ =
  UpdateConfigurationSetTrackingOptionsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucstorsResponseStatus :: Lens.Lens' UpdateConfigurationSetTrackingOptionsResponse Lude.Int
ucstorsResponseStatus = Lens.lens (responseStatus :: UpdateConfigurationSetTrackingOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConfigurationSetTrackingOptionsResponse)
{-# DEPRECATED ucstorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
