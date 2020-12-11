{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkACLEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified ingress or egress entry (rule) from the specified network ACL.
module Network.AWS.EC2.DeleteNetworkACLEntry
  ( -- * Creating a request
    DeleteNetworkACLEntry (..),
    mkDeleteNetworkACLEntry,

    -- ** Request lenses
    dnaeDryRun,
    dnaeEgress,
    dnaeNetworkACLId,
    dnaeRuleNumber,

    -- * Destructuring the response
    DeleteNetworkACLEntryResponse (..),
    mkDeleteNetworkACLEntryResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteNetworkACLEntry' smart constructor.
data DeleteNetworkACLEntry = DeleteNetworkACLEntry'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    egress :: Lude.Bool,
    networkACLId :: Lude.Text,
    ruleNumber :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNetworkACLEntry' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'egress' - Indicates whether the rule is an egress rule.
-- * 'networkACLId' - The ID of the network ACL.
-- * 'ruleNumber' - The rule number of the entry to delete.
mkDeleteNetworkACLEntry ::
  -- | 'egress'
  Lude.Bool ->
  -- | 'networkACLId'
  Lude.Text ->
  -- | 'ruleNumber'
  Lude.Int ->
  DeleteNetworkACLEntry
mkDeleteNetworkACLEntry pEgress_ pNetworkACLId_ pRuleNumber_ =
  DeleteNetworkACLEntry'
    { dryRun = Lude.Nothing,
      egress = pEgress_,
      networkACLId = pNetworkACLId_,
      ruleNumber = pRuleNumber_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaeDryRun :: Lens.Lens' DeleteNetworkACLEntry (Lude.Maybe Lude.Bool)
dnaeDryRun = Lens.lens (dryRun :: DeleteNetworkACLEntry -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteNetworkACLEntry)
{-# DEPRECATED dnaeDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Indicates whether the rule is an egress rule.
--
-- /Note:/ Consider using 'egress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaeEgress :: Lens.Lens' DeleteNetworkACLEntry Lude.Bool
dnaeEgress = Lens.lens (egress :: DeleteNetworkACLEntry -> Lude.Bool) (\s a -> s {egress = a} :: DeleteNetworkACLEntry)
{-# DEPRECATED dnaeEgress "Use generic-lens or generic-optics with 'egress' instead." #-}

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaeNetworkACLId :: Lens.Lens' DeleteNetworkACLEntry Lude.Text
dnaeNetworkACLId = Lens.lens (networkACLId :: DeleteNetworkACLEntry -> Lude.Text) (\s a -> s {networkACLId = a} :: DeleteNetworkACLEntry)
{-# DEPRECATED dnaeNetworkACLId "Use generic-lens or generic-optics with 'networkACLId' instead." #-}

-- | The rule number of the entry to delete.
--
-- /Note:/ Consider using 'ruleNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaeRuleNumber :: Lens.Lens' DeleteNetworkACLEntry Lude.Int
dnaeRuleNumber = Lens.lens (ruleNumber :: DeleteNetworkACLEntry -> Lude.Int) (\s a -> s {ruleNumber = a} :: DeleteNetworkACLEntry)
{-# DEPRECATED dnaeRuleNumber "Use generic-lens or generic-optics with 'ruleNumber' instead." #-}

instance Lude.AWSRequest DeleteNetworkACLEntry where
  type Rs DeleteNetworkACLEntry = DeleteNetworkACLEntryResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteNetworkACLEntryResponse'

instance Lude.ToHeaders DeleteNetworkACLEntry where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteNetworkACLEntry where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNetworkACLEntry where
  toQuery DeleteNetworkACLEntry' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteNetworkAclEntry" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "Egress" Lude.=: egress,
        "NetworkAclId" Lude.=: networkACLId,
        "RuleNumber" Lude.=: ruleNumber
      ]

-- | /See:/ 'mkDeleteNetworkACLEntryResponse' smart constructor.
data DeleteNetworkACLEntryResponse = DeleteNetworkACLEntryResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNetworkACLEntryResponse' with the minimum fields required to make a request.
mkDeleteNetworkACLEntryResponse ::
  DeleteNetworkACLEntryResponse
mkDeleteNetworkACLEntryResponse = DeleteNetworkACLEntryResponse'
