{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between a configuration set and a custom domain for open and click event tracking.
--
-- By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using custom domains, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Amazon SES Developer Guide> .
module Network.AWS.SES.CreateConfigurationSetTrackingOptions
  ( -- * Creating a request
    CreateConfigurationSetTrackingOptions (..),
    mkCreateConfigurationSetTrackingOptions,

    -- ** Request lenses
    ccstoConfigurationSetName,
    ccstoTrackingOptions,

    -- * Destructuring the response
    CreateConfigurationSetTrackingOptionsResponse (..),
    mkCreateConfigurationSetTrackingOptionsResponse,

    -- ** Response lenses
    ccstorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to create an open and click tracking option object in a configuration set.
--
-- /See:/ 'mkCreateConfigurationSetTrackingOptions' smart constructor.
data CreateConfigurationSetTrackingOptions = CreateConfigurationSetTrackingOptions'
  { -- | The name of the configuration set that the tracking options should be associated with.
    configurationSetName :: Lude.Text,
    trackingOptions :: TrackingOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConfigurationSetTrackingOptions' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - The name of the configuration set that the tracking options should be associated with.
-- * 'trackingOptions' -
mkCreateConfigurationSetTrackingOptions ::
  -- | 'configurationSetName'
  Lude.Text ->
  -- | 'trackingOptions'
  TrackingOptions ->
  CreateConfigurationSetTrackingOptions
mkCreateConfigurationSetTrackingOptions
  pConfigurationSetName_
  pTrackingOptions_ =
    CreateConfigurationSetTrackingOptions'
      { configurationSetName =
          pConfigurationSetName_,
        trackingOptions = pTrackingOptions_
      }

-- | The name of the configuration set that the tracking options should be associated with.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccstoConfigurationSetName :: Lens.Lens' CreateConfigurationSetTrackingOptions Lude.Text
ccstoConfigurationSetName = Lens.lens (configurationSetName :: CreateConfigurationSetTrackingOptions -> Lude.Text) (\s a -> s {configurationSetName = a} :: CreateConfigurationSetTrackingOptions)
{-# DEPRECATED ccstoConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trackingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccstoTrackingOptions :: Lens.Lens' CreateConfigurationSetTrackingOptions TrackingOptions
ccstoTrackingOptions = Lens.lens (trackingOptions :: CreateConfigurationSetTrackingOptions -> TrackingOptions) (\s a -> s {trackingOptions = a} :: CreateConfigurationSetTrackingOptions)
{-# DEPRECATED ccstoTrackingOptions "Use generic-lens or generic-optics with 'trackingOptions' instead." #-}

instance Lude.AWSRequest CreateConfigurationSetTrackingOptions where
  type
    Rs CreateConfigurationSetTrackingOptions =
      CreateConfigurationSetTrackingOptionsResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "CreateConfigurationSetTrackingOptionsResult"
      ( \s h x ->
          CreateConfigurationSetTrackingOptionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConfigurationSetTrackingOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateConfigurationSetTrackingOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConfigurationSetTrackingOptions where
  toQuery CreateConfigurationSetTrackingOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateConfigurationSetTrackingOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSetName" Lude.=: configurationSetName,
        "TrackingOptions" Lude.=: trackingOptions
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateConfigurationSetTrackingOptionsResponse' smart constructor.
newtype CreateConfigurationSetTrackingOptionsResponse = CreateConfigurationSetTrackingOptionsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateConfigurationSetTrackingOptionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateConfigurationSetTrackingOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConfigurationSetTrackingOptionsResponse
mkCreateConfigurationSetTrackingOptionsResponse pResponseStatus_ =
  CreateConfigurationSetTrackingOptionsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccstorsResponseStatus :: Lens.Lens' CreateConfigurationSetTrackingOptionsResponse Lude.Int
ccstorsResponseStatus = Lens.lens (responseStatus :: CreateConfigurationSetTrackingOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConfigurationSetTrackingOptionsResponse)
{-# DEPRECATED ccstorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
