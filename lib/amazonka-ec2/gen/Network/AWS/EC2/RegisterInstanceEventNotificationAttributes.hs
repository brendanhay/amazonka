{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RegisterInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a set of tag keys to include in scheduled event notifications for your resources.
--
-- To remove tags, use .
module Network.AWS.EC2.RegisterInstanceEventNotificationAttributes
  ( -- * Creating a request
    RegisterInstanceEventNotificationAttributes (..),
    mkRegisterInstanceEventNotificationAttributes,

    -- ** Request lenses
    rienaInstanceTagAttribute,
    rienaDryRun,

    -- * Destructuring the response
    RegisterInstanceEventNotificationAttributesResponse (..),
    mkRegisterInstanceEventNotificationAttributesResponse,

    -- ** Response lenses
    rienarsInstanceTagAttribute,
    rienarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterInstanceEventNotificationAttributes' smart constructor.
data RegisterInstanceEventNotificationAttributes = RegisterInstanceEventNotificationAttributes'
  { instanceTagAttribute ::
      Lude.Maybe
        RegisterInstanceTagAttributeRequest,
    dryRun ::
      Lude.Maybe
        Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterInstanceEventNotificationAttributes' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceTagAttribute' - Information about the tag keys to register.
mkRegisterInstanceEventNotificationAttributes ::
  RegisterInstanceEventNotificationAttributes
mkRegisterInstanceEventNotificationAttributes =
  RegisterInstanceEventNotificationAttributes'
    { instanceTagAttribute =
        Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Information about the tag keys to register.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rienaInstanceTagAttribute :: Lens.Lens' RegisterInstanceEventNotificationAttributes (Lude.Maybe RegisterInstanceTagAttributeRequest)
rienaInstanceTagAttribute = Lens.lens (instanceTagAttribute :: RegisterInstanceEventNotificationAttributes -> Lude.Maybe RegisterInstanceTagAttributeRequest) (\s a -> s {instanceTagAttribute = a} :: RegisterInstanceEventNotificationAttributes)
{-# DEPRECATED rienaInstanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rienaDryRun :: Lens.Lens' RegisterInstanceEventNotificationAttributes (Lude.Maybe Lude.Bool)
rienaDryRun = Lens.lens (dryRun :: RegisterInstanceEventNotificationAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RegisterInstanceEventNotificationAttributes)
{-# DEPRECATED rienaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Lude.AWSRequest
    RegisterInstanceEventNotificationAttributes
  where
  type
    Rs RegisterInstanceEventNotificationAttributes =
      RegisterInstanceEventNotificationAttributesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RegisterInstanceEventNotificationAttributesResponse'
            Lude.<$> (x Lude..@? "instanceTagAttribute")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterInstanceEventNotificationAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RegisterInstanceEventNotificationAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterInstanceEventNotificationAttributes where
  toQuery RegisterInstanceEventNotificationAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RegisterInstanceEventNotificationAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceTagAttribute" Lude.=: instanceTagAttribute,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkRegisterInstanceEventNotificationAttributesResponse' smart constructor.
data RegisterInstanceEventNotificationAttributesResponse = RegisterInstanceEventNotificationAttributesResponse'
  { instanceTagAttribute ::
      Lude.Maybe
        InstanceTagNotificationAttribute,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'RegisterInstanceEventNotificationAttributesResponse' with the minimum fields required to make a request.
--
-- * 'instanceTagAttribute' - The resulting set of tag keys.
-- * 'responseStatus' - The response status code.
mkRegisterInstanceEventNotificationAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterInstanceEventNotificationAttributesResponse
mkRegisterInstanceEventNotificationAttributesResponse
  pResponseStatus_ =
    RegisterInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The resulting set of tag keys.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rienarsInstanceTagAttribute :: Lens.Lens' RegisterInstanceEventNotificationAttributesResponse (Lude.Maybe InstanceTagNotificationAttribute)
rienarsInstanceTagAttribute = Lens.lens (instanceTagAttribute :: RegisterInstanceEventNotificationAttributesResponse -> Lude.Maybe InstanceTagNotificationAttribute) (\s a -> s {instanceTagAttribute = a} :: RegisterInstanceEventNotificationAttributesResponse)
{-# DEPRECATED rienarsInstanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rienarsResponseStatus :: Lens.Lens' RegisterInstanceEventNotificationAttributesResponse Lude.Int
rienarsResponseStatus = Lens.lens (responseStatus :: RegisterInstanceEventNotificationAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterInstanceEventNotificationAttributesResponse)
{-# DEPRECATED rienarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
