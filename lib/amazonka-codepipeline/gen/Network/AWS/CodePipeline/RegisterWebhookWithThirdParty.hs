{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures a connection between the webhook that was created and the external tool with events to be detected.
module Network.AWS.CodePipeline.RegisterWebhookWithThirdParty
  ( -- * Creating a request
    RegisterWebhookWithThirdParty (..),
    mkRegisterWebhookWithThirdParty,

    -- ** Request lenses
    rwwtpWebhookName,

    -- * Destructuring the response
    RegisterWebhookWithThirdPartyResponse (..),
    mkRegisterWebhookWithThirdPartyResponse,

    -- ** Response lenses
    rwwtprsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterWebhookWithThirdParty' smart constructor.
newtype RegisterWebhookWithThirdParty = RegisterWebhookWithThirdParty'
  { webhookName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterWebhookWithThirdParty' with the minimum fields required to make a request.
--
-- * 'webhookName' - The name of an existing webhook created with PutWebhook to register with a supported third party.
mkRegisterWebhookWithThirdParty ::
  RegisterWebhookWithThirdParty
mkRegisterWebhookWithThirdParty =
  RegisterWebhookWithThirdParty' {webhookName = Lude.Nothing}

-- | The name of an existing webhook created with PutWebhook to register with a supported third party.
--
-- /Note:/ Consider using 'webhookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwwtpWebhookName :: Lens.Lens' RegisterWebhookWithThirdParty (Lude.Maybe Lude.Text)
rwwtpWebhookName = Lens.lens (webhookName :: RegisterWebhookWithThirdParty -> Lude.Maybe Lude.Text) (\s a -> s {webhookName = a} :: RegisterWebhookWithThirdParty)
{-# DEPRECATED rwwtpWebhookName "Use generic-lens or generic-optics with 'webhookName' instead." #-}

instance Lude.AWSRequest RegisterWebhookWithThirdParty where
  type
    Rs RegisterWebhookWithThirdParty =
      RegisterWebhookWithThirdPartyResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RegisterWebhookWithThirdPartyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterWebhookWithThirdParty where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.RegisterWebhookWithThirdParty" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterWebhookWithThirdParty where
  toJSON RegisterWebhookWithThirdParty' {..} =
    Lude.object
      (Lude.catMaybes [("webhookName" Lude..=) Lude.<$> webhookName])

instance Lude.ToPath RegisterWebhookWithThirdParty where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterWebhookWithThirdParty where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterWebhookWithThirdPartyResponse' smart constructor.
newtype RegisterWebhookWithThirdPartyResponse = RegisterWebhookWithThirdPartyResponse'
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
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterWebhookWithThirdPartyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRegisterWebhookWithThirdPartyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterWebhookWithThirdPartyResponse
mkRegisterWebhookWithThirdPartyResponse pResponseStatus_ =
  RegisterWebhookWithThirdPartyResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwwtprsResponseStatus :: Lens.Lens' RegisterWebhookWithThirdPartyResponse Lude.Int
rwwtprsResponseStatus = Lens.lens (responseStatus :: RegisterWebhookWithThirdPartyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterWebhookWithThirdPartyResponse)
{-# DEPRECATED rwwtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
