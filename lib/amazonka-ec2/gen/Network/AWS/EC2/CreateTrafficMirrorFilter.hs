{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTrafficMirrorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Traffic Mirror filter.
--
-- A Traffic Mirror filter is a set of rules that defines the traffic to mirror.
-- By default, no traffic is mirrored. To mirror traffic, use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorFilterRule.htm CreateTrafficMirrorFilterRule> to add Traffic Mirror rules to the filter. The rules you add define what traffic gets mirrored. You can also use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ModifyTrafficMirrorFilterNetworkServices.html ModifyTrafficMirrorFilterNetworkServices> to mirror supported network services.
module Network.AWS.EC2.CreateTrafficMirrorFilter
  ( -- * Creating a request
    CreateTrafficMirrorFilter (..),
    mkCreateTrafficMirrorFilter,

    -- ** Request lenses
    ctmfClientToken,
    ctmfTagSpecifications,
    ctmfDescription,
    ctmfDryRun,

    -- * Destructuring the response
    CreateTrafficMirrorFilterResponse (..),
    mkCreateTrafficMirrorFilterResponse,

    -- ** Response lenses
    ctmfrsClientToken,
    ctmfrsTrafficMirrorFilter,
    ctmfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTrafficMirrorFilter' smart constructor.
data CreateTrafficMirrorFilter = CreateTrafficMirrorFilter'
  { clientToken ::
      Lude.Maybe Lude.Text,
    tagSpecifications ::
      Lude.Maybe [TagSpecification],
    description :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficMirrorFilter' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'description' - The description of the Traffic Mirror filter.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'tagSpecifications' - The tags to assign to a Traffic Mirror filter.
mkCreateTrafficMirrorFilter ::
  CreateTrafficMirrorFilter
mkCreateTrafficMirrorFilter =
  CreateTrafficMirrorFilter'
    { clientToken = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      description = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfClientToken :: Lens.Lens' CreateTrafficMirrorFilter (Lude.Maybe Lude.Text)
ctmfClientToken = Lens.lens (clientToken :: CreateTrafficMirrorFilter -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateTrafficMirrorFilter)
{-# DEPRECATED ctmfClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The tags to assign to a Traffic Mirror filter.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfTagSpecifications :: Lens.Lens' CreateTrafficMirrorFilter (Lude.Maybe [TagSpecification])
ctmfTagSpecifications = Lens.lens (tagSpecifications :: CreateTrafficMirrorFilter -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateTrafficMirrorFilter)
{-# DEPRECATED ctmfTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The description of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfDescription :: Lens.Lens' CreateTrafficMirrorFilter (Lude.Maybe Lude.Text)
ctmfDescription = Lens.lens (description :: CreateTrafficMirrorFilter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateTrafficMirrorFilter)
{-# DEPRECATED ctmfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfDryRun :: Lens.Lens' CreateTrafficMirrorFilter (Lude.Maybe Lude.Bool)
ctmfDryRun = Lens.lens (dryRun :: CreateTrafficMirrorFilter -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateTrafficMirrorFilter)
{-# DEPRECATED ctmfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateTrafficMirrorFilter where
  type
    Rs CreateTrafficMirrorFilter =
      CreateTrafficMirrorFilterResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateTrafficMirrorFilterResponse'
            Lude.<$> (x Lude..@? "clientToken")
            Lude.<*> (x Lude..@? "trafficMirrorFilter")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTrafficMirrorFilter where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateTrafficMirrorFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrafficMirrorFilter where
  toQuery CreateTrafficMirrorFilter' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateTrafficMirrorFilter" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientToken" Lude.=: clientToken,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "Description" Lude.=: description,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkCreateTrafficMirrorFilterResponse' smart constructor.
data CreateTrafficMirrorFilterResponse = CreateTrafficMirrorFilterResponse'
  { clientToken ::
      Lude.Maybe Lude.Text,
    trafficMirrorFilter ::
      Lude.Maybe
        TrafficMirrorFilter,
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrafficMirrorFilterResponse' with the minimum fields required to make a request.
--
-- * 'clientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
-- * 'responseStatus' - The response status code.
-- * 'trafficMirrorFilter' - Information about the Traffic Mirror filter.
mkCreateTrafficMirrorFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrafficMirrorFilterResponse
mkCreateTrafficMirrorFilterResponse pResponseStatus_ =
  CreateTrafficMirrorFilterResponse'
    { clientToken = Lude.Nothing,
      trafficMirrorFilter = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrsClientToken :: Lens.Lens' CreateTrafficMirrorFilterResponse (Lude.Maybe Lude.Text)
ctmfrsClientToken = Lens.lens (clientToken :: CreateTrafficMirrorFilterResponse -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateTrafficMirrorFilterResponse)
{-# DEPRECATED ctmfrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Information about the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrsTrafficMirrorFilter :: Lens.Lens' CreateTrafficMirrorFilterResponse (Lude.Maybe TrafficMirrorFilter)
ctmfrsTrafficMirrorFilter = Lens.lens (trafficMirrorFilter :: CreateTrafficMirrorFilterResponse -> Lude.Maybe TrafficMirrorFilter) (\s a -> s {trafficMirrorFilter = a} :: CreateTrafficMirrorFilterResponse)
{-# DEPRECATED ctmfrsTrafficMirrorFilter "Use generic-lens or generic-optics with 'trafficMirrorFilter' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctmfrsResponseStatus :: Lens.Lens' CreateTrafficMirrorFilterResponse Lude.Int
ctmfrsResponseStatus = Lens.lens (responseStatus :: CreateTrafficMirrorFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrafficMirrorFilterResponse)
{-# DEPRECATED ctmfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
