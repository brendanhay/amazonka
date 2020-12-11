{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RegisterThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions a thing in the device registry. RegisterThing calls other AWS IoT control plane APIs. These calls might exceed your account level <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_iot AWS IoT Throttling Limits> and cause throttle errors. Please contact <https://console.aws.amazon.com/support/home AWS Customer Support> to raise your throttling limits if necessary.
module Network.AWS.IoT.RegisterThing
  ( -- * Creating a request
    RegisterThing (..),
    mkRegisterThing,

    -- ** Request lenses
    rtParameters,
    rtTemplateBody,

    -- * Destructuring the response
    RegisterThingResponse (..),
    mkRegisterThingResponse,

    -- ** Response lenses
    rtrsCertificatePem,
    rtrsResourceARNs,
    rtrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterThing' smart constructor.
data RegisterThing = RegisterThing'
  { parameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    templateBody :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterThing' with the minimum fields required to make a request.
--
-- * 'parameters' - The parameters for provisioning a thing. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates> for more information.
-- * 'templateBody' - The provisioning template. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates> for more information.
mkRegisterThing ::
  -- | 'templateBody'
  Lude.Text ->
  RegisterThing
mkRegisterThing pTemplateBody_ =
  RegisterThing'
    { parameters = Lude.Nothing,
      templateBody = pTemplateBody_
    }

-- | The parameters for provisioning a thing. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning Templates> for more information.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtParameters :: Lens.Lens' RegisterThing (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rtParameters = Lens.lens (parameters :: RegisterThing -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: RegisterThing)
{-# DEPRECATED rtParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The provisioning template. See <https://docs.aws.amazon.com/iot/latest/developerguide/provision-w-cert.html Provisioning Devices That Have Device Certificates> for more information.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTemplateBody :: Lens.Lens' RegisterThing Lude.Text
rtTemplateBody = Lens.lens (templateBody :: RegisterThing -> Lude.Text) (\s a -> s {templateBody = a} :: RegisterThing)
{-# DEPRECATED rtTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

instance Lude.AWSRequest RegisterThing where
  type Rs RegisterThing = RegisterThingResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          RegisterThingResponse'
            Lude.<$> (x Lude..?> "certificatePem")
            Lude.<*> (x Lude..?> "resourceArns" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterThing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RegisterThing where
  toJSON RegisterThing' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("templateBody" Lude..= templateBody)
          ]
      )

instance Lude.ToPath RegisterThing where
  toPath = Lude.const "/things"

instance Lude.ToQuery RegisterThing where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterThingResponse' smart constructor.
data RegisterThingResponse = RegisterThingResponse'
  { certificatePem ::
      Lude.Maybe Lude.Text,
    resourceARNs ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterThingResponse' with the minimum fields required to make a request.
--
-- * 'certificatePem' - The certificate data, in PEM format.
-- * 'resourceARNs' - ARNs for the generated resources.
-- * 'responseStatus' - The response status code.
mkRegisterThingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterThingResponse
mkRegisterThingResponse pResponseStatus_ =
  RegisterThingResponse'
    { certificatePem = Lude.Nothing,
      resourceARNs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsCertificatePem :: Lens.Lens' RegisterThingResponse (Lude.Maybe Lude.Text)
rtrsCertificatePem = Lens.lens (certificatePem :: RegisterThingResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificatePem = a} :: RegisterThingResponse)
{-# DEPRECATED rtrsCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | ARNs for the generated resources.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsResourceARNs :: Lens.Lens' RegisterThingResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rtrsResourceARNs = Lens.lens (resourceARNs :: RegisterThingResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {resourceARNs = a} :: RegisterThingResponse)
{-# DEPRECATED rtrsResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsResponseStatus :: Lens.Lens' RegisterThingResponse Lude.Int
rtrsResponseStatus = Lens.lens (responseStatus :: RegisterThingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterThingResponse)
{-# DEPRECATED rtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
