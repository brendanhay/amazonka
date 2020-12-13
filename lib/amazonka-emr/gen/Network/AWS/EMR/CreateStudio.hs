{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.CreateStudio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon EMR Studio.
module Network.AWS.EMR.CreateStudio
  ( -- * Creating a request
    CreateStudio (..),
    mkCreateStudio,

    -- ** Request lenses
    csfEngineSecurityGroupId,
    csfSubnetIds,
    csfVPCId,
    csfAuthMode,
    csfDefaultS3Location,
    csfWorkspaceSecurityGroupId,
    csfName,
    csfUserRole,
    csfDescription,
    csfTags,
    csfServiceRole,

    -- * Destructuring the response
    CreateStudioResponse (..),
    mkCreateStudioResponse,

    -- ** Response lenses
    crsStudioId,
    crsURL,
    crsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateStudio' smart constructor.
data CreateStudio = CreateStudio'
  { -- | The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
    engineSecurityGroupId :: Lude.Text,
    -- | A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
    subnetIds :: [Lude.Text],
    -- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
    vpcId :: Lude.Text,
    -- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
    authMode :: AuthMode,
    -- | The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
    defaultS3Location :: Lude.Maybe Lude.Text,
    -- | The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
    workspaceSecurityGroupId :: Lude.Text,
    -- | A descriptive name for the Amazon EMR Studio.
    name :: Lude.Text,
    -- | The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
    userRole :: Lude.Text,
    -- | A detailed description of the Studio.
    description :: Lude.Maybe Lude.Text,
    -- | A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
    tags :: Lude.Maybe [Tag],
    -- | The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
    serviceRole :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStudio' with the minimum fields required to make a request.
--
-- * 'engineSecurityGroupId' - The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
-- * 'subnetIds' - A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
-- * 'vpcId' - The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
-- * 'authMode' - Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
-- * 'defaultS3Location' - The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
-- * 'workspaceSecurityGroupId' - The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
-- * 'name' - A descriptive name for the Amazon EMR Studio.
-- * 'userRole' - The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
-- * 'description' - A detailed description of the Studio.
-- * 'tags' - A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
-- * 'serviceRole' - The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
mkCreateStudio ::
  -- | 'engineSecurityGroupId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  -- | 'authMode'
  AuthMode ->
  -- | 'workspaceSecurityGroupId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'userRole'
  Lude.Text ->
  -- | 'serviceRole'
  Lude.Text ->
  CreateStudio
mkCreateStudio
  pEngineSecurityGroupId_
  pVPCId_
  pAuthMode_
  pWorkspaceSecurityGroupId_
  pName_
  pUserRole_
  pServiceRole_ =
    CreateStudio'
      { engineSecurityGroupId = pEngineSecurityGroupId_,
        subnetIds = Lude.mempty,
        vpcId = pVPCId_,
        authMode = pAuthMode_,
        defaultS3Location = Lude.Nothing,
        workspaceSecurityGroupId = pWorkspaceSecurityGroupId_,
        name = pName_,
        userRole = pUserRole_,
        description = Lude.Nothing,
        tags = Lude.Nothing,
        serviceRole = pServiceRole_
      }

-- | The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
--
-- /Note:/ Consider using 'engineSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfEngineSecurityGroupId :: Lens.Lens' CreateStudio Lude.Text
csfEngineSecurityGroupId = Lens.lens (engineSecurityGroupId :: CreateStudio -> Lude.Text) (\s a -> s {engineSecurityGroupId = a} :: CreateStudio)
{-# DEPRECATED csfEngineSecurityGroupId "Use generic-lens or generic-optics with 'engineSecurityGroupId' instead." #-}

-- | A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfSubnetIds :: Lens.Lens' CreateStudio [Lude.Text]
csfSubnetIds = Lens.lens (subnetIds :: CreateStudio -> [Lude.Text]) (\s a -> s {subnetIds = a} :: CreateStudio)
{-# DEPRECATED csfSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfVPCId :: Lens.Lens' CreateStudio Lude.Text
csfVPCId = Lens.lens (vpcId :: CreateStudio -> Lude.Text) (\s a -> s {vpcId = a} :: CreateStudio)
{-# DEPRECATED csfVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfAuthMode :: Lens.Lens' CreateStudio AuthMode
csfAuthMode = Lens.lens (authMode :: CreateStudio -> AuthMode) (\s a -> s {authMode = a} :: CreateStudio)
{-# DEPRECATED csfAuthMode "Use generic-lens or generic-optics with 'authMode' instead." #-}

-- | The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
--
-- /Note:/ Consider using 'defaultS3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDefaultS3Location :: Lens.Lens' CreateStudio (Lude.Maybe Lude.Text)
csfDefaultS3Location = Lens.lens (defaultS3Location :: CreateStudio -> Lude.Maybe Lude.Text) (\s a -> s {defaultS3Location = a} :: CreateStudio)
{-# DEPRECATED csfDefaultS3Location "Use generic-lens or generic-optics with 'defaultS3Location' instead." #-}

-- | The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
--
-- /Note:/ Consider using 'workspaceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfWorkspaceSecurityGroupId :: Lens.Lens' CreateStudio Lude.Text
csfWorkspaceSecurityGroupId = Lens.lens (workspaceSecurityGroupId :: CreateStudio -> Lude.Text) (\s a -> s {workspaceSecurityGroupId = a} :: CreateStudio)
{-# DEPRECATED csfWorkspaceSecurityGroupId "Use generic-lens or generic-optics with 'workspaceSecurityGroupId' instead." #-}

-- | A descriptive name for the Amazon EMR Studio.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfName :: Lens.Lens' CreateStudio Lude.Text
csfName = Lens.lens (name :: CreateStudio -> Lude.Text) (\s a -> s {name = a} :: CreateStudio)
{-# DEPRECATED csfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfUserRole :: Lens.Lens' CreateStudio Lude.Text
csfUserRole = Lens.lens (userRole :: CreateStudio -> Lude.Text) (\s a -> s {userRole = a} :: CreateStudio)
{-# DEPRECATED csfUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

-- | A detailed description of the Studio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDescription :: Lens.Lens' CreateStudio (Lude.Maybe Lude.Text)
csfDescription = Lens.lens (description :: CreateStudio -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateStudio)
{-# DEPRECATED csfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfTags :: Lens.Lens' CreateStudio (Lude.Maybe [Tag])
csfTags = Lens.lens (tags :: CreateStudio -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateStudio)
{-# DEPRECATED csfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfServiceRole :: Lens.Lens' CreateStudio Lude.Text
csfServiceRole = Lens.lens (serviceRole :: CreateStudio -> Lude.Text) (\s a -> s {serviceRole = a} :: CreateStudio)
{-# DEPRECATED csfServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.AWSRequest CreateStudio where
  type Rs CreateStudio = CreateStudioResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateStudioResponse'
            Lude.<$> (x Lude..?> "StudioId")
            Lude.<*> (x Lude..?> "Url")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateStudio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.CreateStudio" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateStudio where
  toJSON CreateStudio' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("EngineSecurityGroupId" Lude..= engineSecurityGroupId),
            Lude.Just ("SubnetIds" Lude..= subnetIds),
            Lude.Just ("VpcId" Lude..= vpcId),
            Lude.Just ("AuthMode" Lude..= authMode),
            ("DefaultS3Location" Lude..=) Lude.<$> defaultS3Location,
            Lude.Just
              ("WorkspaceSecurityGroupId" Lude..= workspaceSecurityGroupId),
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("UserRole" Lude..= userRole),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("ServiceRole" Lude..= serviceRole)
          ]
      )

instance Lude.ToPath CreateStudio where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStudio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStudioResponse' smart constructor.
data CreateStudioResponse = CreateStudioResponse'
  { -- | The ID of the Amazon EMR Studio.
    studioId :: Lude.Maybe Lude.Text,
    -- | The unique Studio access URL.
    url :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStudioResponse' with the minimum fields required to make a request.
--
-- * 'studioId' - The ID of the Amazon EMR Studio.
-- * 'url' - The unique Studio access URL.
-- * 'responseStatus' - The response status code.
mkCreateStudioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateStudioResponse
mkCreateStudioResponse pResponseStatus_ =
  CreateStudioResponse'
    { studioId = Lude.Nothing,
      url = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsStudioId :: Lens.Lens' CreateStudioResponse (Lude.Maybe Lude.Text)
crsStudioId = Lens.lens (studioId :: CreateStudioResponse -> Lude.Maybe Lude.Text) (\s a -> s {studioId = a} :: CreateStudioResponse)
{-# DEPRECATED crsStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | The unique Studio access URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsURL :: Lens.Lens' CreateStudioResponse (Lude.Maybe Lude.Text)
crsURL = Lens.lens (url :: CreateStudioResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: CreateStudioResponse)
{-# DEPRECATED crsURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateStudioResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateStudioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateStudioResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
