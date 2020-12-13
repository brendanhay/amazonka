{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Updates the description of an egress (outbound) security group rule. You can replace an existing description, or add a description to a rule that did not have one previously.
--
-- You specify the description as part of the IP permissions structure. You can remove a description for a security group rule by omitting the description parameter in the request.
module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
  ( -- * Creating a request
    UpdateSecurityGroupRuleDescriptionsEgress (..),
    mkUpdateSecurityGroupRuleDescriptionsEgress,

    -- ** Request lenses
    usgrdeIPPermissions,
    usgrdeGroupId,
    usgrdeGroupName,
    usgrdeDryRun,

    -- * Destructuring the response
    UpdateSecurityGroupRuleDescriptionsEgressResponse (..),
    mkUpdateSecurityGroupRuleDescriptionsEgressResponse,

    -- ** Response lenses
    usgrdersReturn,
    usgrdersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSecurityGroupRuleDescriptionsEgress' smart constructor.
data UpdateSecurityGroupRuleDescriptionsEgress = UpdateSecurityGroupRuleDescriptionsEgress'
  { -- | The IP permissions for the security group rule.
    ipPermissions :: [IPPermission],
    -- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
    groupId :: Lude.Maybe Lude.Text,
    -- | [Default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
    groupName :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSecurityGroupRuleDescriptionsEgress' with the minimum fields required to make a request.
--
-- * 'ipPermissions' - The IP permissions for the security group rule.
-- * 'groupId' - The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
-- * 'groupName' - [Default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkUpdateSecurityGroupRuleDescriptionsEgress ::
  UpdateSecurityGroupRuleDescriptionsEgress
mkUpdateSecurityGroupRuleDescriptionsEgress =
  UpdateSecurityGroupRuleDescriptionsEgress'
    { ipPermissions =
        Lude.mempty,
      groupId = Lude.Nothing,
      groupName = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The IP permissions for the security group rule.
--
-- /Note:/ Consider using 'ipPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdeIPPermissions :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress [IPPermission]
usgrdeIPPermissions = Lens.lens (ipPermissions :: UpdateSecurityGroupRuleDescriptionsEgress -> [IPPermission]) (\s a -> s {ipPermissions = a} :: UpdateSecurityGroupRuleDescriptionsEgress)
{-# DEPRECATED usgrdeIPPermissions "Use generic-lens or generic-optics with 'ipPermissions' instead." #-}

-- | The ID of the security group. You must specify either the security group ID or the security group name in the request. For security groups in a nondefault VPC, you must specify the security group ID.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdeGroupId :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress (Lude.Maybe Lude.Text)
usgrdeGroupId = Lens.lens (groupId :: UpdateSecurityGroupRuleDescriptionsEgress -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: UpdateSecurityGroupRuleDescriptionsEgress)
{-# DEPRECATED usgrdeGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | [Default VPC] The name of the security group. You must specify either the security group ID or the security group name in the request.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdeGroupName :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress (Lude.Maybe Lude.Text)
usgrdeGroupName = Lens.lens (groupName :: UpdateSecurityGroupRuleDescriptionsEgress -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: UpdateSecurityGroupRuleDescriptionsEgress)
{-# DEPRECATED usgrdeGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdeDryRun :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress (Lude.Maybe Lude.Bool)
usgrdeDryRun = Lens.lens (dryRun :: UpdateSecurityGroupRuleDescriptionsEgress -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: UpdateSecurityGroupRuleDescriptionsEgress)
{-# DEPRECATED usgrdeDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest UpdateSecurityGroupRuleDescriptionsEgress where
  type
    Rs UpdateSecurityGroupRuleDescriptionsEgress =
      UpdateSecurityGroupRuleDescriptionsEgressResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          UpdateSecurityGroupRuleDescriptionsEgressResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSecurityGroupRuleDescriptionsEgress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateSecurityGroupRuleDescriptionsEgress where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSecurityGroupRuleDescriptionsEgress where
  toQuery UpdateSecurityGroupRuleDescriptionsEgress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateSecurityGroupRuleDescriptionsEgress" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQueryList "IpPermissions" ipPermissions,
        "GroupId" Lude.=: groupId,
        "GroupName" Lude.=: groupName,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkUpdateSecurityGroupRuleDescriptionsEgressResponse' smart constructor.
data UpdateSecurityGroupRuleDescriptionsEgressResponse = UpdateSecurityGroupRuleDescriptionsEgressResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSecurityGroupRuleDescriptionsEgressResponse' with the minimum fields required to make a request.
--
-- * 'return' - Returns @true@ if the request succeeds; otherwise, returns an error.
-- * 'responseStatus' - The response status code.
mkUpdateSecurityGroupRuleDescriptionsEgressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSecurityGroupRuleDescriptionsEgressResponse
mkUpdateSecurityGroupRuleDescriptionsEgressResponse
  pResponseStatus_ =
    UpdateSecurityGroupRuleDescriptionsEgressResponse'
      { return =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdersReturn :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgressResponse (Lude.Maybe Lude.Bool)
usgrdersReturn = Lens.lens (return :: UpdateSecurityGroupRuleDescriptionsEgressResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: UpdateSecurityGroupRuleDescriptionsEgressResponse)
{-# DEPRECATED usgrdersReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrdersResponseStatus :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgressResponse Lude.Int
usgrdersResponseStatus = Lens.lens (responseStatus :: UpdateSecurityGroupRuleDescriptionsEgressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSecurityGroupRuleDescriptionsEgressResponse)
{-# DEPRECATED usgrdersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
