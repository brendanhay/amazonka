{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters tag keys to prevent tags that have the specified tag keys from being included in scheduled event notifications for resources in the Region.
module Network.AWS.EC2.DeregisterInstanceEventNotificationAttributes
  ( -- * Creating a request
    DeregisterInstanceEventNotificationAttributes (..),
    mkDeregisterInstanceEventNotificationAttributes,

    -- ** Request lenses
    dienasInstanceTagAttribute,
    dienasDryRun,

    -- * Destructuring the response
    DeregisterInstanceEventNotificationAttributesResponse (..),
    mkDeregisterInstanceEventNotificationAttributesResponse,

    -- ** Response lenses
    dienarsInstanceTagAttribute,
    dienarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterInstanceEventNotificationAttributes' smart constructor.
data DeregisterInstanceEventNotificationAttributes = DeregisterInstanceEventNotificationAttributes'
  { -- | Information about the tag keys to deregister.
    instanceTagAttribute :: Lude.Maybe DeregisterInstanceTagAttributeRequest,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterInstanceEventNotificationAttributes' with the minimum fields required to make a request.
--
-- * 'instanceTagAttribute' - Information about the tag keys to deregister.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeregisterInstanceEventNotificationAttributes ::
  DeregisterInstanceEventNotificationAttributes
mkDeregisterInstanceEventNotificationAttributes =
  DeregisterInstanceEventNotificationAttributes'
    { instanceTagAttribute =
        Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Information about the tag keys to deregister.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienasInstanceTagAttribute :: Lens.Lens' DeregisterInstanceEventNotificationAttributes (Lude.Maybe DeregisterInstanceTagAttributeRequest)
dienasInstanceTagAttribute = Lens.lens (instanceTagAttribute :: DeregisterInstanceEventNotificationAttributes -> Lude.Maybe DeregisterInstanceTagAttributeRequest) (\s a -> s {instanceTagAttribute = a} :: DeregisterInstanceEventNotificationAttributes)
{-# DEPRECATED dienasInstanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienasDryRun :: Lens.Lens' DeregisterInstanceEventNotificationAttributes (Lude.Maybe Lude.Bool)
dienasDryRun = Lens.lens (dryRun :: DeregisterInstanceEventNotificationAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeregisterInstanceEventNotificationAttributes)
{-# DEPRECATED dienasDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Lude.AWSRequest
    DeregisterInstanceEventNotificationAttributes
  where
  type
    Rs DeregisterInstanceEventNotificationAttributes =
      DeregisterInstanceEventNotificationAttributesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeregisterInstanceEventNotificationAttributesResponse'
            Lude.<$> (x Lude..@? "instanceTagAttribute")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DeregisterInstanceEventNotificationAttributes
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeregisterInstanceEventNotificationAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterInstanceEventNotificationAttributes where
  toQuery DeregisterInstanceEventNotificationAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ( "DeregisterInstanceEventNotificationAttributes" ::
                      Lude.ByteString
                  ),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceTagAttribute" Lude.=: instanceTagAttribute,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeregisterInstanceEventNotificationAttributesResponse' smart constructor.
data DeregisterInstanceEventNotificationAttributesResponse = DeregisterInstanceEventNotificationAttributesResponse'
  { -- | The resulting set of tag keys.
    instanceTagAttribute :: Lude.Maybe InstanceTagNotificationAttribute,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterInstanceEventNotificationAttributesResponse' with the minimum fields required to make a request.
--
-- * 'instanceTagAttribute' - The resulting set of tag keys.
-- * 'responseStatus' - The response status code.
mkDeregisterInstanceEventNotificationAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterInstanceEventNotificationAttributesResponse
mkDeregisterInstanceEventNotificationAttributesResponse
  pResponseStatus_ =
    DeregisterInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The resulting set of tag keys.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienarsInstanceTagAttribute :: Lens.Lens' DeregisterInstanceEventNotificationAttributesResponse (Lude.Maybe InstanceTagNotificationAttribute)
dienarsInstanceTagAttribute = Lens.lens (instanceTagAttribute :: DeregisterInstanceEventNotificationAttributesResponse -> Lude.Maybe InstanceTagNotificationAttribute) (\s a -> s {instanceTagAttribute = a} :: DeregisterInstanceEventNotificationAttributesResponse)
{-# DEPRECATED dienarsInstanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienarsResponseStatus :: Lens.Lens' DeregisterInstanceEventNotificationAttributesResponse Lude.Int
dienarsResponseStatus = Lens.lens (responseStatus :: DeregisterInstanceEventNotificationAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterInstanceEventNotificationAttributesResponse)
{-# DEPRECATED dienarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
