{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.DeleteConfigurationSetTrackingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an association between a configuration set and a custom domain for open and click event tracking.
--
-- By default, images and links used for tracking open and click events are hosted on domains operated by Amazon SES. You can configure a subdomain of your own to handle these events. For information about using custom domains, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/configure-custom-open-click-domains.html Amazon SES Developer Guide> .
module Network.AWS.SES.DeleteConfigurationSetTrackingOptions
  ( -- * Creating a request
    DeleteConfigurationSetTrackingOptions (..),
    mkDeleteConfigurationSetTrackingOptions,

    -- ** Request lenses
    dcstoConfigurationSetName,

    -- * Destructuring the response
    DeleteConfigurationSetTrackingOptionsResponse (..),
    mkDeleteConfigurationSetTrackingOptionsResponse,

    -- ** Response lenses
    dcstorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to delete open and click tracking options in a configuration set.
--
-- /See:/ 'mkDeleteConfigurationSetTrackingOptions' smart constructor.
newtype DeleteConfigurationSetTrackingOptions = DeleteConfigurationSetTrackingOptions'
  { configurationSetName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConfigurationSetTrackingOptions' with the minimum fields required to make a request.
--
-- * 'configurationSetName' - The name of the configuration set from which you want to delete the tracking options.
mkDeleteConfigurationSetTrackingOptions ::
  -- | 'configurationSetName'
  Lude.Text ->
  DeleteConfigurationSetTrackingOptions
mkDeleteConfigurationSetTrackingOptions pConfigurationSetName_ =
  DeleteConfigurationSetTrackingOptions'
    { configurationSetName =
        pConfigurationSetName_
    }

-- | The name of the configuration set from which you want to delete the tracking options.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcstoConfigurationSetName :: Lens.Lens' DeleteConfigurationSetTrackingOptions Lude.Text
dcstoConfigurationSetName = Lens.lens (configurationSetName :: DeleteConfigurationSetTrackingOptions -> Lude.Text) (\s a -> s {configurationSetName = a} :: DeleteConfigurationSetTrackingOptions)
{-# DEPRECATED dcstoConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

instance Lude.AWSRequest DeleteConfigurationSetTrackingOptions where
  type
    Rs DeleteConfigurationSetTrackingOptions =
      DeleteConfigurationSetTrackingOptionsResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "DeleteConfigurationSetTrackingOptionsResult"
      ( \s h x ->
          DeleteConfigurationSetTrackingOptionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConfigurationSetTrackingOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteConfigurationSetTrackingOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConfigurationSetTrackingOptions where
  toQuery DeleteConfigurationSetTrackingOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteConfigurationSetTrackingOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ConfigurationSetName" Lude.=: configurationSetName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkDeleteConfigurationSetTrackingOptionsResponse' smart constructor.
newtype DeleteConfigurationSetTrackingOptionsResponse = DeleteConfigurationSetTrackingOptionsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DeleteConfigurationSetTrackingOptionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteConfigurationSetTrackingOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConfigurationSetTrackingOptionsResponse
mkDeleteConfigurationSetTrackingOptionsResponse pResponseStatus_ =
  DeleteConfigurationSetTrackingOptionsResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcstorsResponseStatus :: Lens.Lens' DeleteConfigurationSetTrackingOptionsResponse Lude.Int
dcstorsResponseStatus = Lens.lens (responseStatus :: DeleteConfigurationSetTrackingOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConfigurationSetTrackingOptionsResponse)
{-# DEPRECATED dcstorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
