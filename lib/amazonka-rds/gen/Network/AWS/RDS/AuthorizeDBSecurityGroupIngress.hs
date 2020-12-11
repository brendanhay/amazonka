{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables ingress to a DBSecurityGroup using one of two forms of authorization. First, EC2 or VPC security groups can be added to the DBSecurityGroup if the application using the database is running on EC2 or VPC instances. Second, IP ranges are available if the application accessing your database is running on the Internet. Required parameters for this API are one of CIDR range, EC2SecurityGroupId for VPC, or (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or EC2SecurityGroupId for non-VPC).
--
-- For an overview of CIDR ranges, go to the <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Wikipedia Tutorial> .
module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
  ( -- * Creating a request
    AuthorizeDBSecurityGroupIngress (..),
    mkAuthorizeDBSecurityGroupIngress,

    -- ** Request lenses
    adsgiEC2SecurityGroupOwnerId,
    adsgiEC2SecurityGroupName,
    adsgiCIdRIP,
    adsgiEC2SecurityGroupId,
    adsgiDBSecurityGroupName,

    -- * Destructuring the response
    AuthorizeDBSecurityGroupIngressResponse (..),
    mkAuthorizeDBSecurityGroupIngressResponse,

    -- ** Response lenses
    adsgirsDBSecurityGroup,
    adsgirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkAuthorizeDBSecurityGroupIngress' smart constructor.
data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress'
  { ec2SecurityGroupOwnerId ::
      Lude.Maybe Lude.Text,
    ec2SecurityGroupName ::
      Lude.Maybe Lude.Text,
    cIdRIP ::
      Lude.Maybe Lude.Text,
    ec2SecurityGroupId ::
      Lude.Maybe Lude.Text,
    dbSecurityGroupName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizeDBSecurityGroupIngress' with the minimum fields required to make a request.
--
-- * 'cIdRIP' - The IP range to authorize.
-- * 'dbSecurityGroupName' - The name of the DB security group to add authorization to.
-- * 'ec2SecurityGroupId' - Id of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
-- * 'ec2SecurityGroupName' - Name of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
-- * 'ec2SecurityGroupOwnerId' - AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
mkAuthorizeDBSecurityGroupIngress ::
  -- | 'dbSecurityGroupName'
  Lude.Text ->
  AuthorizeDBSecurityGroupIngress
mkAuthorizeDBSecurityGroupIngress pDBSecurityGroupName_ =
  AuthorizeDBSecurityGroupIngress'
    { ec2SecurityGroupOwnerId =
        Lude.Nothing,
      ec2SecurityGroupName = Lude.Nothing,
      cIdRIP = Lude.Nothing,
      ec2SecurityGroupId = Lude.Nothing,
      dbSecurityGroupName = pDBSecurityGroupName_
    }

-- | AWS account number of the owner of the EC2 security group specified in the @EC2SecurityGroupName@ parameter. The AWS access key ID isn't an acceptable value. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- /Note:/ Consider using 'ec2SecurityGroupOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsgiEC2SecurityGroupOwnerId :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Lude.Maybe Lude.Text)
adsgiEC2SecurityGroupOwnerId = Lens.lens (ec2SecurityGroupOwnerId :: AuthorizeDBSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupOwnerId = a} :: AuthorizeDBSecurityGroupIngress)
{-# DEPRECATED adsgiEC2SecurityGroupOwnerId "Use generic-lens or generic-optics with 'ec2SecurityGroupOwnerId' instead." #-}

-- | Name of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- /Note:/ Consider using 'ec2SecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsgiEC2SecurityGroupName :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Lude.Maybe Lude.Text)
adsgiEC2SecurityGroupName = Lens.lens (ec2SecurityGroupName :: AuthorizeDBSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupName = a} :: AuthorizeDBSecurityGroupIngress)
{-# DEPRECATED adsgiEC2SecurityGroupName "Use generic-lens or generic-optics with 'ec2SecurityGroupName' instead." #-}

-- | The IP range to authorize.
--
-- /Note:/ Consider using 'cIdRIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsgiCIdRIP :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Lude.Maybe Lude.Text)
adsgiCIdRIP = Lens.lens (cIdRIP :: AuthorizeDBSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {cIdRIP = a} :: AuthorizeDBSecurityGroupIngress)
{-# DEPRECATED adsgiCIdRIP "Use generic-lens or generic-optics with 'cIdRIP' instead." #-}

-- | Id of the EC2 security group to authorize. For VPC DB security groups, @EC2SecurityGroupId@ must be provided. Otherwise, @EC2SecurityGroupOwnerId@ and either @EC2SecurityGroupName@ or @EC2SecurityGroupId@ must be provided.
--
-- /Note:/ Consider using 'ec2SecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsgiEC2SecurityGroupId :: Lens.Lens' AuthorizeDBSecurityGroupIngress (Lude.Maybe Lude.Text)
adsgiEC2SecurityGroupId = Lens.lens (ec2SecurityGroupId :: AuthorizeDBSecurityGroupIngress -> Lude.Maybe Lude.Text) (\s a -> s {ec2SecurityGroupId = a} :: AuthorizeDBSecurityGroupIngress)
{-# DEPRECATED adsgiEC2SecurityGroupId "Use generic-lens or generic-optics with 'ec2SecurityGroupId' instead." #-}

-- | The name of the DB security group to add authorization to.
--
-- /Note:/ Consider using 'dbSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsgiDBSecurityGroupName :: Lens.Lens' AuthorizeDBSecurityGroupIngress Lude.Text
adsgiDBSecurityGroupName = Lens.lens (dbSecurityGroupName :: AuthorizeDBSecurityGroupIngress -> Lude.Text) (\s a -> s {dbSecurityGroupName = a} :: AuthorizeDBSecurityGroupIngress)
{-# DEPRECATED adsgiDBSecurityGroupName "Use generic-lens or generic-optics with 'dbSecurityGroupName' instead." #-}

instance Lude.AWSRequest AuthorizeDBSecurityGroupIngress where
  type
    Rs AuthorizeDBSecurityGroupIngress =
      AuthorizeDBSecurityGroupIngressResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "AuthorizeDBSecurityGroupIngressResult"
      ( \s h x ->
          AuthorizeDBSecurityGroupIngressResponse'
            Lude.<$> (x Lude..@? "DBSecurityGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AuthorizeDBSecurityGroupIngress where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AuthorizeDBSecurityGroupIngress where
  toPath = Lude.const "/"

instance Lude.ToQuery AuthorizeDBSecurityGroupIngress where
  toQuery AuthorizeDBSecurityGroupIngress' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AuthorizeDBSecurityGroupIngress" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EC2SecurityGroupOwnerId" Lude.=: ec2SecurityGroupOwnerId,
        "EC2SecurityGroupName" Lude.=: ec2SecurityGroupName,
        "CIDRIP" Lude.=: cIdRIP,
        "EC2SecurityGroupId" Lude.=: ec2SecurityGroupId,
        "DBSecurityGroupName" Lude.=: dbSecurityGroupName
      ]

-- | /See:/ 'mkAuthorizeDBSecurityGroupIngressResponse' smart constructor.
data AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse'
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

-- | Creates a value of 'AuthorizeDBSecurityGroupIngressResponse' with the minimum fields required to make a request.
--
-- * 'dbSecurityGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkAuthorizeDBSecurityGroupIngressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AuthorizeDBSecurityGroupIngressResponse
mkAuthorizeDBSecurityGroupIngressResponse pResponseStatus_ =
  AuthorizeDBSecurityGroupIngressResponse'
    { dbSecurityGroup =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSecurityGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsgirsDBSecurityGroup :: Lens.Lens' AuthorizeDBSecurityGroupIngressResponse (Lude.Maybe DBSecurityGroup)
adsgirsDBSecurityGroup = Lens.lens (dbSecurityGroup :: AuthorizeDBSecurityGroupIngressResponse -> Lude.Maybe DBSecurityGroup) (\s a -> s {dbSecurityGroup = a} :: AuthorizeDBSecurityGroupIngressResponse)
{-# DEPRECATED adsgirsDBSecurityGroup "Use generic-lens or generic-optics with 'dbSecurityGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adsgirsResponseStatus :: Lens.Lens' AuthorizeDBSecurityGroupIngressResponse Lude.Int
adsgirsResponseStatus = Lens.lens (responseStatus :: AuthorizeDBSecurityGroupIngressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AuthorizeDBSecurityGroupIngressResponse)
{-# DEPRECATED adsgirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
