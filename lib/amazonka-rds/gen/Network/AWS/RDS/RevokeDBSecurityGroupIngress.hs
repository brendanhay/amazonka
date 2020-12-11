{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RevokeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes ingress from a DBSecurityGroup for previously authorized IP ranges or EC2 or VPC Security Groups. Required parameters for this API are one of CIDRIP, EC2SecurityGroupId for VPC, or (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or EC2SecurityGroupId).
module Network.AWS.RDS.RevokeDBSecurityGroupIngress
  ( -- * Creating a request
    RevokeDBSecurityGroupIngress (..),
    mkRevokeDBSecurityGroupIngress,

    -- ** Request lenses
    rdsgiEC2SecurityGroupOwnerId,
    rdsgiEC2SecurityGroupName,
    rdsgiCIdRIP,
    rdsgiEC2SecurityGroupId,
    rdsgiDBSecurityGroupName,

    -- * Destructuring the response
    RevokeDBSecurityGroupIngressResponse (..),
    mkRevokeDBSecurityGroupIngressResponse,

    -- ** Response lenses
    rdsgirsDBSecurityGroup,
    rdsgirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkRevokeDBSecurityGroupIngress' smart constructor.
data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress'
  { ec2SecurityGroupOwnerId ::
      Lude.Maybe Lude.Text,
    ec2SecurityGroupName ::
      Lude.Maybe Lude.Text,
    cIdRIP :: Lude.Maybe Lude.Text,
    ec2SecurityGroupId ::
      Lude.Maybe Lude.Text,
    dbSecurityGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeDBSecurityGroupIngress' with the minimum fields required to make a request.
--
-- * 'cIdRIP' - The IP range to revoke access from. Must be a valid CIDR range. If @CIDRIP@ is specified, @EC2SecurityGroupName@ , @EC2SecurityGroupId@ and @EC2SecurityGroupOwnerId@ can't be provided.
-- * 'dbSecurityGroupName' - The name of the DB security group to revoke ingress from.
-- * 'ec2SecurityGroupId' - The id of the EC2 security group to revoke access from. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
-- * 'ec2SecurityGroupName' - The name of the EC2 security group to revoke access from. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
-- * 'ec2SecurityGroupOwnerId' - The AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
mkRevokeDBSecurityGroupIngress ::
  -- | 'dbSecurityGroupName'
  Lude.Text ->
  RevokeDBSecurityGroupIngress
mkRevokeDBSecurityGroupIngress pDBSecurityGroupName_ =
  RevokeDBSecurityGroupIngress'
    { ec2SecurityGroupOwnerId =
        Lude.Nothing,
      ec2SecurityGroupName = Lude.Nothing,
      cIdRIP = Lude.Nothing,
      ec2SecurityGroupId = Lude.Nothing,
      dbSecurityGroupName = pDBSecurityGroupName_
    }

-- | The AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- /Note:/ Consider using 'ec2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsgiEC2SecurityGroupOwnerId :: Lens.Lens' RevokeDBSecurityGroupIngress (Lude.Maybe Lude.Text)
rdsgiEC2SecurityGroupOwnerId = Lens.lens (ec2SecurityGroupOwnerId :: RevokeDBSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupOwnerId = a} :: RevokeDBSecurityGroupIngress)
{-# DEPRECATED rdsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'ec2SecurityGroupOwnerId' instead." #-}

-- | The name of the EC2 security group to revoke access from. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- /Note:/ Consider using 'ec2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsgiEC2SecurityGroupName :: Lens.Lens' RevokeDBSecurityGroupIngress (Lude.Maybe Lude.Text)
rdsgiEC2SecurityGroupName = Lens.lens (ec2SecurityGroupName :: RevokeDBSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupName = a} :: RevokeDBSecurityGroupIngress)
{-# DEPRECATED rdsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'ec2SecurityGroupName' instead." #-}

-- | The IP range to revoke access from. Must be a valid CIDR range. If @CIDRIP@ is specified, @EC2SecurityGroupName@ , @EC2SecurityGroupId@ and @EC2SecurityGroupOwnerId@ can't be provided.
--
-- /Note:/ Consider using 'cIdRIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsgiCIdRIP :: Lens.Lens' RevokeDBSecurityGroupIngress (Lude.Maybe Lude.Text)
rdsgiCIdRIP = Lens.lens (cIdRIP :: RevokeDBSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {cIdRIP = a} :: RevokeDBSecurityGroupIngress)
{-# DEPRECATED rdsgiCIdRIP "Use generic-lens or generic-optics with 'cIdRIP' instead." #-}

-- | The id of the EC2 security group to revoke access from. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, EC2SecurityGroupOwnerId and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- /Note:/ Consider using 'ec2SecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsgiEC2SecurityGroupId :: Lens.Lens' RevokeDBSecurityGroupIngress (Lude.Maybe Lude.Text)
rdsgiEC2SecurityGroupId = Lens.lens (ec2SecurityGroupId :: RevokeDBSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupId = a} :: RevokeDBSecurityGroupIngress)
{-# DEPRECATED rdsgiEC2SecurityGroupId "Use generic-lens or generic-optics with 'ec2SecurityGroupId' instead." #-}

-- | The name of the DB security group to revoke ingress from.
--
-- /Note:/ Consider using 'dbSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsgiDBSecurityGroupName :: Lens.Lens' RevokeDBSecurityGroupIngress Lude.Text
rdsgiDBSecurityGroupName = Lens.lens (dbSecurityGroupName :: RevokeDBSecurityGroupIngress -> Lude.Text) (\s a -> s {dbSecurityGroupName = a} :: RevokeDBSecurityGroupIngress)
{-# DEPRECATED rdsgiDBSecurityGroupName "Use generic-lens or generic-optics with 'dbSecurityGroupName' instead." #-}

instance Lude.AWSRequest RevokeDBSecurityGroupIngress where
  type
    Rs RevokeDBSecurityGroupIngress =
      RevokeDBSecurityGroupIngressResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "RevokeDBSecurityGroupIngressResult"
      ( \s h x ->
          RevokeDBSecurityGroupIngressResponse'
            Lude.<$> (x Lude..@? "DBSecurityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RevokeDBSecurityGroupIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RevokeDBSecurityGroupIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeDBSecurityGroupIngress where
  toQuery RevokeDBSecurityGroupIngress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("RevokeDBSecurityGroupIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EC2SecurityGroupOwnerId" Lude.=: ec2SecurityGroupOwnerId,
        "EC2SecurityGroupName" Lude.=: ec2SecurityGroupName,
        "CIDRIP" Lude.=: cIdRIP,
        "EC2SecurityGroupId" Lude.=: ec2SecurityGroupId,
        "DBSecurityGroupName" Lude.=: dbSecurityGroupName
      ]

-- | /See:/ 'mkRevokeDBSecurityGroupIngressResponse' smart constructor.
data RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse'
  { dbSecurityGroup ::
      Lude.Maybe
        DBSecurityGroup,
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

-- | Creates a value of 'RevokeDBSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- * 'dbSecurityGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkRevokeDBSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RevokeDBSecurityGroupIngressResponse
mkRevokeDBSecurityGroupIngressResponse pResponseStatus_ =
  RevokeDBSecurityGroupIngressResponse'
    { dbSecurityGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsgirsDBSecurityGroup :: Lens.Lens' RevokeDBSecurityGroupIngressResponse (Lude.Maybe DBSecurityGroup)
rdsgirsDBSecurityGroup = Lens.lens (dbSecurityGroup :: RevokeDBSecurityGroupIngressResponse -> Lude.Maybe DBSecurityGroup) (\s a -> s {dbSecurityGroup = a} :: RevokeDBSecurityGroupIngressResponse)
{-# DEPRECATED rdsgirsDBSecurityGroup "Use generic-lens or generic-optics with 'dbSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsgirsResponseStatus :: Lens.Lens' RevokeDBSecurityGroupIngressResponse Lude.Int
rdsgirsResponseStatus = Lens.lens (responseStatus :: RevokeDBSecurityGroupIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RevokeDBSecurityGroupIngressResponse)
{-# DEPRECATED rdsgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
