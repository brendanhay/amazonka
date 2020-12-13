{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the connection between the webhook that was created by CodePipeline and the external tool with events to be detected. Currently supported only for webhooks that target an action type of GitHub.
module Network.AWS.CodePipeline.DeregisterWebhookWithThirdParty
  ( -- * Creating a request
    DeregisterWebhookWithThirdParty (..),
    mkDeregisterWebhookWithThirdParty,

    -- ** Request lenses
    dwwtpWebhookName,

    -- * Destructuring the response
    DeregisterWebhookWithThirdPartyResponse (..),
    mkDeregisterWebhookWithThirdPartyResponse,

    -- ** Response lenses
    dwwtprsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterWebhookWithThirdParty' smart constructor.
newtype DeregisterWebhookWithThirdParty = DeregisterWebhookWithThirdParty'
  { -- | The name of the webhook you want to deregister.
    webhookName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterWebhookWithThirdParty' with the minimum fields required to make a request.
--
-- * 'webhookName' - The name of the webhook you want to deregister.
mkDeregisterWebhookWithThirdParty ::
  DeregisterWebhookWithThirdParty
mkDeregisterWebhookWithThirdParty =
  DeregisterWebhookWithThirdParty' {webhookName = Lude.Nothing}

-- | The name of the webhook you want to deregister.
--
-- /Note:/ Consider using 'webhookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwwtpWebhookName :: Lens.Lens' DeregisterWebhookWithThirdParty (Lude.Maybe Lude.Text)
dwwtpWebhookName = Lens.lens (webhookName :: DeregisterWebhookWithThirdParty -> Lude.Maybe Lude.Text) (\s a -> s {webhookName = a} :: DeregisterWebhookWithThirdParty)
{-# DEPRECATED dwwtpWebhookName "Use generic-lens or generic-optics with 'webhookName' instead." #-}

instance Lude.AWSRequest DeregisterWebhookWithThirdParty where
  type
    Rs DeregisterWebhookWithThirdParty =
      DeregisterWebhookWithThirdPartyResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeregisterWebhookWithThirdPartyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterWebhookWithThirdParty where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.DeregisterWebhookWithThirdParty" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterWebhookWithThirdParty where
  toJSON DeregisterWebhookWithThirdParty' {..} =
    Lude.object
      (Lude.catMaybes [("webhookName" Lude..=) Lude.<$> webhookName])

instance Lude.ToPath DeregisterWebhookWithThirdParty where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterWebhookWithThirdParty where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterWebhookWithThirdPartyResponse' smart constructor.
newtype DeregisterWebhookWithThirdPartyResponse = DeregisterWebhookWithThirdPartyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterWebhookWithThirdPartyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeregisterWebhookWithThirdPartyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterWebhookWithThirdPartyResponse
mkDeregisterWebhookWithThirdPartyResponse pResponseStatus_ =
  DeregisterWebhookWithThirdPartyResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwwtprsResponseStatus :: Lens.Lens' DeregisterWebhookWithThirdPartyResponse Lude.Int
dwwtprsResponseStatus = Lens.lens (responseStatus :: DeregisterWebhookWithThirdPartyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterWebhookWithThirdPartyResponse)
{-# DEPRECATED dwwtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
