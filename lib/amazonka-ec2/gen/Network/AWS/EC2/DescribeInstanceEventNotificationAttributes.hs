{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tag keys that are registered to appear in scheduled event notifications for resources in the current Region.
module Network.AWS.EC2.DescribeInstanceEventNotificationAttributes
  ( -- * Creating a request
    DescribeInstanceEventNotificationAttributes (..),
    mkDescribeInstanceEventNotificationAttributes,

    -- ** Request lenses
    dienasDryRun,

    -- * Destructuring the response
    DescribeInstanceEventNotificationAttributesResponse (..),
    mkDescribeInstanceEventNotificationAttributesResponse,

    -- ** Response lenses
    dienasrsInstanceTagAttribute,
    dienasrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceEventNotificationAttributes' smart constructor.
newtype DescribeInstanceEventNotificationAttributes = DescribeInstanceEventNotificationAttributes'
  { dryRun ::
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
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeInstanceEventNotificationAttributes' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeInstanceEventNotificationAttributes ::
  DescribeInstanceEventNotificationAttributes
mkDescribeInstanceEventNotificationAttributes =
  DescribeInstanceEventNotificationAttributes'
    { dryRun =
        Lude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienasDryRun :: Lens.Lens' DescribeInstanceEventNotificationAttributes (Lude.Maybe Lude.Bool)
dienasDryRun = Lens.lens (dryRun :: DescribeInstanceEventNotificationAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeInstanceEventNotificationAttributes)
{-# DEPRECATED dienasDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance
  Lude.AWSRequest
    DescribeInstanceEventNotificationAttributes
  where
  type
    Rs DescribeInstanceEventNotificationAttributes =
      DescribeInstanceEventNotificationAttributesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeInstanceEventNotificationAttributesResponse'
            Lude.<$> (x Lude..@? "instanceTagAttribute")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceEventNotificationAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstanceEventNotificationAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceEventNotificationAttributes where
  toQuery DescribeInstanceEventNotificationAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeInstanceEventNotificationAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeInstanceEventNotificationAttributesResponse' smart constructor.
data DescribeInstanceEventNotificationAttributesResponse = DescribeInstanceEventNotificationAttributesResponse'
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

-- | Creates a value of 'DescribeInstanceEventNotificationAttributesResponse' with the minimum fields required to make a request.
--
-- * 'instanceTagAttribute' - Information about the registered tag keys.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceEventNotificationAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceEventNotificationAttributesResponse
mkDescribeInstanceEventNotificationAttributesResponse
  pResponseStatus_ =
    DescribeInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Information about the registered tag keys.
--
-- /Note:/ Consider using 'instanceTagAttribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienasrsInstanceTagAttribute :: Lens.Lens' DescribeInstanceEventNotificationAttributesResponse (Lude.Maybe InstanceTagNotificationAttribute)
dienasrsInstanceTagAttribute = Lens.lens (instanceTagAttribute :: DescribeInstanceEventNotificationAttributesResponse -> Lude.Maybe InstanceTagNotificationAttribute) (\s a -> s {instanceTagAttribute = a} :: DescribeInstanceEventNotificationAttributesResponse)
{-# DEPRECATED dienasrsInstanceTagAttribute "Use generic-lens or generic-optics with 'instanceTagAttribute' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dienasrsResponseStatus :: Lens.Lens' DescribeInstanceEventNotificationAttributesResponse Lude.Int
dienasrsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceEventNotificationAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceEventNotificationAttributesResponse)
{-# DEPRECATED dienasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
