{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyAvailabilityZoneGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the opt-in status of the Local Zone and Wavelength Zone group for your account.
--
-- Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeAvailabilityZones.html DescribeAvailabilityZones> to view the value for @GroupName@ .
module Network.AWS.EC2.ModifyAvailabilityZoneGroup
  ( -- * Creating a request
    ModifyAvailabilityZoneGroup (..),
    mkModifyAvailabilityZoneGroup,

    -- ** Request lenses
    mazgDryRun,
    mazgGroupName,
    mazgOptInStatus,

    -- * Destructuring the response
    ModifyAvailabilityZoneGroupResponse (..),
    mkModifyAvailabilityZoneGroupResponse,

    -- ** Response lenses
    mazgrsReturn,
    mazgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyAvailabilityZoneGroup' smart constructor.
data ModifyAvailabilityZoneGroup = ModifyAvailabilityZoneGroup'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    groupName :: Lude.Text,
    optInStatus ::
      ModifyAvailabilityZoneOptInStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyAvailabilityZoneGroup' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'groupName' - The name of the Availability Zone group, Local Zone group, or Wavelength Zone group.
-- * 'optInStatus' - Indicates whether you are opted in to the Local Zone group or Wavelength Zone group. The only valid value is @opted-in@ . You must contact <https://console.aws.amazon.com/support/home#/case/create%3FissueType=customer-service%26serviceCode=general-info%26getting-started%26categoryCode=using-aws%26services AWS Support> to opt out of a Local Zone group, or Wavelength Zone group.
mkModifyAvailabilityZoneGroup ::
  -- | 'groupName'
  Lude.Text ->
  -- | 'optInStatus'
  ModifyAvailabilityZoneOptInStatus ->
  ModifyAvailabilityZoneGroup
mkModifyAvailabilityZoneGroup pGroupName_ pOptInStatus_ =
  ModifyAvailabilityZoneGroup'
    { dryRun = Lude.Nothing,
      groupName = pGroupName_,
      optInStatus = pOptInStatus_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgDryRun :: Lens.Lens' ModifyAvailabilityZoneGroup (Lude.Maybe Lude.Bool)
mazgDryRun = Lens.lens (dryRun :: ModifyAvailabilityZoneGroup -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyAvailabilityZoneGroup)
{-# DEPRECATED mazgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The name of the Availability Zone group, Local Zone group, or Wavelength Zone group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgGroupName :: Lens.Lens' ModifyAvailabilityZoneGroup Lude.Text
mazgGroupName = Lens.lens (groupName :: ModifyAvailabilityZoneGroup -> Lude.Text) (\s a -> s {groupName = a} :: ModifyAvailabilityZoneGroup)
{-# DEPRECATED mazgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Indicates whether you are opted in to the Local Zone group or Wavelength Zone group. The only valid value is @opted-in@ . You must contact <https://console.aws.amazon.com/support/home#/case/create%3FissueType=customer-service%26serviceCode=general-info%26getting-started%26categoryCode=using-aws%26services AWS Support> to opt out of a Local Zone group, or Wavelength Zone group.
--
-- /Note:/ Consider using 'optInStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgOptInStatus :: Lens.Lens' ModifyAvailabilityZoneGroup ModifyAvailabilityZoneOptInStatus
mazgOptInStatus = Lens.lens (optInStatus :: ModifyAvailabilityZoneGroup -> ModifyAvailabilityZoneOptInStatus) (\s a -> s {optInStatus = a} :: ModifyAvailabilityZoneGroup)
{-# DEPRECATED mazgOptInStatus "Use generic-lens or generic-optics with 'optInStatus' instead." #-}

instance Lude.AWSRequest ModifyAvailabilityZoneGroup where
  type
    Rs ModifyAvailabilityZoneGroup =
      ModifyAvailabilityZoneGroupResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyAvailabilityZoneGroupResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyAvailabilityZoneGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyAvailabilityZoneGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyAvailabilityZoneGroup where
  toQuery ModifyAvailabilityZoneGroup' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyAvailabilityZoneGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "GroupName" Lude.=: groupName,
        "OptInStatus" Lude.=: optInStatus
      ]

-- | /See:/ 'mkModifyAvailabilityZoneGroupResponse' smart constructor.
data ModifyAvailabilityZoneGroupResponse = ModifyAvailabilityZoneGroupResponse'
  { return ::
      Lude.Maybe
        Lude.Bool,
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

-- | Creates a value of 'ModifyAvailabilityZoneGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Is @true@ if the request succeeds, and an error otherwise.
mkModifyAvailabilityZoneGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyAvailabilityZoneGroupResponse
mkModifyAvailabilityZoneGroupResponse pResponseStatus_ =
  ModifyAvailabilityZoneGroupResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgrsReturn :: Lens.Lens' ModifyAvailabilityZoneGroupResponse (Lude.Maybe Lude.Bool)
mazgrsReturn = Lens.lens (return :: ModifyAvailabilityZoneGroupResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: ModifyAvailabilityZoneGroupResponse)
{-# DEPRECATED mazgrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mazgrsResponseStatus :: Lens.Lens' ModifyAvailabilityZoneGroupResponse Lude.Int
mazgrsResponseStatus = Lens.lens (responseStatus :: ModifyAvailabilityZoneGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyAvailabilityZoneGroupResponse)
{-# DEPRECATED mazgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
