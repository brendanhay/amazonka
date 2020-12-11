{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of an ingress (inbound) security group rule. You can replace an existing description, or add a description to a rule that did not have one previously.
--
-- You specify the description as part of the IP permissions structure. You can remove a description for a security group rule by omitting the description parameter in the request.
module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsIngress
  ( -- * Creating a request
    UpdateSecurityGroupRuleDescriptionsIngress (..),
    mkUpdateSecurityGroupRuleDescriptionsIngress,

    -- ** Request lenses
    usgrdiGroupId,
    usgrdiGroupName,
    usgrdiDryRun,
    usgrdiIPPermissions,

    -- * Destructuring the response
    UpdateSecurityGroupRuleDescriptionsIngressResponse (..),
    mkUpdateSecurityGroupRuleDescriptionsIngressResponse,

    -- ** Response lenses
    usgrdirsReturn,
    usgrdirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSecurityGroupRuleDescriptionsIngress' smart constructor.
data UpdateSecurityGroupRuleDescriptionsIngress = UpdateSecurityGroupRuleDescriptionsIngress'
  { groupId ::
      Lude.Maybe
        Lude.Text,
    groupName ::
      Lude.Maybe
        Lude.Text,
    dryRun ::
      Lude.Maybe
        Lude.Bool,
    ipPermissions ::
      [IPPermission]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSecurityGroupRuleDescriptionsIngress' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'groupId' - The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
-- * 'groupName' - [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
-- * 'ipPermissions' - The IP permissions for the security group rule.
mkUpdateSecurityGroupRuleDescriptionsIngress ::
  UpdateSecurityGroupRuleDescriptionsIngress
mkUpdateSecurityGroupRuleDescriptionsIngress =
  UpdateSecurityGroupRuleDescriptionsIngress'
    { groupId =
        Lude.Nothing,
      groupName = Lude.Nothing,
      dryRun = Lude.Nothing,
      ipPermissions = Lude.mempty
    }

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdiGroupId :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Lude.Maybe Lude.Text)
usgrdiGroupId = Lens.lens (groupId :: UpdateSecurityGroupRuleDescriptionsIngress -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: UpdateSecurityGroupRuleDescriptionsIngress)
{-# DEPRECATED usgrdiGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | [EC2-Classic, default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdiGroupName :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Lude.Maybe Lude.Text)
usgrdiGroupName = Lens.lens (groupName :: UpdateSecurityGroupRuleDescriptionsIngress -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: UpdateSecurityGroupRuleDescriptionsIngress)
{-# DEPRECATED usgrdiGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdiDryRun :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Lude.Maybe Lude.Bool)
usgrdiDryRun = Lens.lens (dryRun :: UpdateSecurityGroupRuleDescriptionsIngress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: UpdateSecurityGroupRuleDescriptionsIngress)
{-# DEPRECATED usgrdiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The IP permissions for the security group rule.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdiIPPermissions :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress [IPPermission]
usgrdiIPPermissions = Lens.lens (ipPermissions :: UpdateSecurityGroupRuleDescriptionsIngress -> [IPPermission]) (\s a -> s {ipPermissions = a} :: UpdateSecurityGroupRuleDescriptionsIngress)
{-# DEPRECATED usgrdiIPPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

instance Lude.AWSRequest UpdateSecurityGroupRuleDescriptionsIngress where
  type
    Rs UpdateSecurityGroupRuleDescriptionsIngress =
      UpdateSecurityGroupRuleDescriptionsIngressResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateSecurityGroupRuleDescriptionsIngressResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSecurityGroupRuleDescriptionsIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateSecurityGroupRuleDescriptionsIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSecurityGroupRuleDescriptionsIngress where
  toQuery UpdateSecurityGroupRuleDescriptionsIngress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateSecurityGroupRuleDescriptionsIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "GroupId" Lude.=: groupId,
        "GroupName" Lude.=: groupName,
        "DryRun" Lude.=: dryRun,
        Lude.toQueryList "IpPermissions" ipPermissions
      ]

-- | /See:/ 'mkUpdateSecurityGroupRuleDescriptionsIngressResponse' smart constructor.
data UpdateSecurityGroupRuleDescriptionsIngressResponse = UpdateSecurityGroupRuleDescriptionsIngressResponse'
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'UpdateSecurityGroupRuleDescriptionsIngressResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, returns an error.
mkUpdateSecurityGroupRuleDescriptionsIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSecurityGroupRuleDescriptionsIngressResponse
mkUpdateSecurityGroupRuleDescriptionsIngressResponse
  pResponseStatus_ =
    UpdateSecurityGroupRuleDescriptionsIngressResponse'
      { return =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdirsReturn :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngressResponse (Lude.Maybe Lude.Bool)
usgrdirsReturn = Lens.lens (return :: UpdateSecurityGroupRuleDescriptionsIngressResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: UpdateSecurityGroupRuleDescriptionsIngressResponse)
{-# DEPRECATED usgrdirsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdirsResponseStatus :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngressResponse Lude.Int
usgrdirsResponseStatus = Lens.lens (responseStatus :: UpdateSecurityGroupRuleDescriptionsIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSecurityGroupRuleDescriptionsIngressResponse)
{-# DEPRECATED usgrdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
