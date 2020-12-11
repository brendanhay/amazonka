{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    csDefaultS3Location,
    csDescription,
    csTags,
    csName,
    csAuthMode,
    csVPCId,
    csSubnetIds,
    csServiceRole,
    csUserRole,
    csWorkspaceSecurityGroupId,
    csEngineSecurityGroupId,

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
  { defaultS3Location ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    authMode :: AuthMode,
    vpcId :: Lude.Text,
    subnetIds :: [Lude.Text],
    serviceRole :: Lude.Text,
    userRole :: Lude.Text,
    workspaceSecurityGroupId :: Lude.Text,
    engineSecurityGroupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStudio' with the minimum fields required to make a request.
--
-- * 'authMode' - Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
-- * 'defaultS3Location' - The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
-- * 'description' - A detailed description of the Studio.
-- * 'engineSecurityGroupId' - The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
-- * 'name' - A descriptive name for the Amazon EMR Studio.
-- * 'serviceRole' - The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
-- * 'subnetIds' - A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
-- * 'tags' - A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
-- * 'userRole' - The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
-- * 'vpcId' - The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
-- * 'workspaceSecurityGroupId' - The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
mkCreateStudio ::
  -- | 'name'
  Lude.Text ->
  -- | 'authMode'
  AuthMode ->
  -- | 'vpcId'
  Lude.Text ->
  -- | 'serviceRole'
  Lude.Text ->
  -- | 'userRole'
  Lude.Text ->
  -- | 'workspaceSecurityGroupId'
  Lude.Text ->
  -- | 'engineSecurityGroupId'
  Lude.Text ->
  CreateStudio
mkCreateStudio
  pName_
  pAuthMode_
  pVPCId_
  pServiceRole_
  pUserRole_
  pWorkspaceSecurityGroupId_
  pEngineSecurityGroupId_ =
    CreateStudio'
      { defaultS3Location = Lude.Nothing,
        description = Lude.Nothing,
        tags = Lude.Nothing,
        name = pName_,
        authMode = pAuthMode_,
        vpcId = pVPCId_,
        subnetIds = Lude.mempty,
        serviceRole = pServiceRole_,
        userRole = pUserRole_,
        workspaceSecurityGroupId = pWorkspaceSecurityGroupId_,
        engineSecurityGroupId = pEngineSecurityGroupId_
      }

-- | The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
--
-- /Note:/ Consider using 'defaultS3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDefaultS3Location :: Lens.Lens' CreateStudio (Lude.Maybe Lude.Text)
csDefaultS3Location = Lens.lens (defaultS3Location :: CreateStudio -> Lude.Maybe Lude.Text) (\s a -> s {defaultS3Location = a} :: CreateStudio)
{-# DEPRECATED csDefaultS3Location "Use generic-lens or generic-optics with 'defaultS3Location' instead." #-}

-- | A detailed description of the Studio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateStudio (Lude.Maybe Lude.Text)
csDescription = Lens.lens (description :: CreateStudio -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateStudio)
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateStudio (Lude.Maybe [Tag])
csTags = Lens.lens (tags :: CreateStudio -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateStudio)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A descriptive name for the Amazon EMR Studio.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateStudio Lude.Text
csName = Lens.lens (name :: CreateStudio -> Lude.Text) (\s a -> s {name = a} :: CreateStudio)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAuthMode :: Lens.Lens' CreateStudio AuthMode
csAuthMode = Lens.lens (authMode :: CreateStudio -> AuthMode) (\s a -> s {authMode = a} :: CreateStudio)
{-# DEPRECATED csAuthMode "Use generic-lens or generic-optics with 'authMode' instead." #-}

-- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csVPCId :: Lens.Lens' CreateStudio Lude.Text
csVPCId = Lens.lens (vpcId :: CreateStudio -> Lude.Text) (\s a -> s {vpcId = a} :: CreateStudio)
{-# DEPRECATED csVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSubnetIds :: Lens.Lens' CreateStudio [Lude.Text]
csSubnetIds = Lens.lens (subnetIds :: CreateStudio -> [Lude.Text]) (\s a -> s {subnetIds = a} :: CreateStudio)
{-# DEPRECATED csSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csServiceRole :: Lens.Lens' CreateStudio Lude.Text
csServiceRole = Lens.lens (serviceRole :: CreateStudio -> Lude.Text) (\s a -> s {serviceRole = a} :: CreateStudio)
{-# DEPRECATED csServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUserRole :: Lens.Lens' CreateStudio Lude.Text
csUserRole = Lens.lens (userRole :: CreateStudio -> Lude.Text) (\s a -> s {userRole = a} :: CreateStudio)
{-# DEPRECATED csUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

-- | The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
--
-- /Note:/ Consider using 'workspaceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csWorkspaceSecurityGroupId :: Lens.Lens' CreateStudio Lude.Text
csWorkspaceSecurityGroupId = Lens.lens (workspaceSecurityGroupId :: CreateStudio -> Lude.Text) (\s a -> s {workspaceSecurityGroupId = a} :: CreateStudio)
{-# DEPRECATED csWorkspaceSecurityGroupId "Use generic-lens or generic-optics with 'workspaceSecurityGroupId' instead." #-}

-- | The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
--
-- /Note:/ Consider using 'engineSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEngineSecurityGroupId :: Lens.Lens' CreateStudio Lude.Text
csEngineSecurityGroupId = Lens.lens (engineSecurityGroupId :: CreateStudio -> Lude.Text) (\s a -> s {engineSecurityGroupId = a} :: CreateStudio)
{-# DEPRECATED csEngineSecurityGroupId "Use generic-lens or generic-optics with 'engineSecurityGroupId' instead." #-}

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
          [ ("DefaultS3Location" Lude..=) Lude.<$> defaultS3Location,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("AuthMode" Lude..= authMode),
            Lude.Just ("VpcId" Lude..= vpcId),
            Lude.Just ("SubnetIds" Lude..= subnetIds),
            Lude.Just ("ServiceRole" Lude..= serviceRole),
            Lude.Just ("UserRole" Lude..= userRole),
            Lude.Just
              ("WorkspaceSecurityGroupId" Lude..= workspaceSecurityGroupId),
            Lude.Just ("EngineSecurityGroupId" Lude..= engineSecurityGroupId)
          ]
      )

instance Lude.ToPath CreateStudio where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateStudio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateStudioResponse' smart constructor.
data CreateStudioResponse = CreateStudioResponse'
  { studioId ::
      Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateStudioResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'studioId' - The ID of the Amazon EMR Studio.
-- * 'url' - The unique Studio access URL.
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
