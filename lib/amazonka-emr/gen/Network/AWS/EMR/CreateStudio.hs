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
    csfName,
    csfAuthMode,
    csfVpcId,
    csfSubnetIds,
    csfServiceRole,
    csfUserRole,
    csfWorkspaceSecurityGroupId,
    csfEngineSecurityGroupId,
    csfDefaultS3Location,
    csfDescription,
    csfTags,

    -- * Destructuring the response
    CreateStudioResponse (..),
    mkCreateStudioResponse,

    -- ** Response lenses
    csrrsStudioId,
    csrrsUrl,
    csrrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStudio' smart constructor.
data CreateStudio = CreateStudio'
  { -- | A descriptive name for the Amazon EMR Studio.
    name :: Types.XmlStringMaxLen256,
    -- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
    authMode :: Types.AuthMode,
    -- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
    vpcId :: Types.XmlStringMaxLen256,
    -- | A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
    subnetIds :: [Types.String],
    -- | The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
    serviceRole :: Types.XmlString,
    -- | The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
    userRole :: Types.XmlString,
    -- | The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
    workspaceSecurityGroupId :: Types.XmlStringMaxLen256,
    -- | The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
    engineSecurityGroupId :: Types.XmlStringMaxLen256,
    -- | The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
    defaultS3Location :: Core.Maybe Types.XmlString,
    -- | A detailed description of the Studio.
    description :: Core.Maybe Types.XmlStringMaxLen256,
    -- | A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStudio' value with any optional fields omitted.
mkCreateStudio ::
  -- | 'name'
  Types.XmlStringMaxLen256 ->
  -- | 'authMode'
  Types.AuthMode ->
  -- | 'vpcId'
  Types.XmlStringMaxLen256 ->
  -- | 'serviceRole'
  Types.XmlString ->
  -- | 'userRole'
  Types.XmlString ->
  -- | 'workspaceSecurityGroupId'
  Types.XmlStringMaxLen256 ->
  -- | 'engineSecurityGroupId'
  Types.XmlStringMaxLen256 ->
  CreateStudio
mkCreateStudio
  name
  authMode
  vpcId
  serviceRole
  userRole
  workspaceSecurityGroupId
  engineSecurityGroupId =
    CreateStudio'
      { name,
        authMode,
        vpcId,
        subnetIds = Core.mempty,
        serviceRole,
        userRole,
        workspaceSecurityGroupId,
        engineSecurityGroupId,
        defaultS3Location = Core.Nothing,
        description = Core.Nothing,
        tags = Core.Nothing
      }

-- | A descriptive name for the Amazon EMR Studio.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfName :: Lens.Lens' CreateStudio Types.XmlStringMaxLen256
csfName = Lens.field @"name"
{-# DEPRECATED csfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfAuthMode :: Lens.Lens' CreateStudio Types.AuthMode
csfAuthMode = Lens.field @"authMode"
{-# DEPRECATED csfAuthMode "Use generic-lens or generic-optics with 'authMode' instead." #-}

-- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfVpcId :: Lens.Lens' CreateStudio Types.XmlStringMaxLen256
csfVpcId = Lens.field @"vpcId"
{-# DEPRECATED csfVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfSubnetIds :: Lens.Lens' CreateStudio [Types.String]
csfSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED csfSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfServiceRole :: Lens.Lens' CreateStudio Types.XmlString
csfServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED csfServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfUserRole :: Lens.Lens' CreateStudio Types.XmlString
csfUserRole = Lens.field @"userRole"
{-# DEPRECATED csfUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

-- | The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
--
-- /Note:/ Consider using 'workspaceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfWorkspaceSecurityGroupId :: Lens.Lens' CreateStudio Types.XmlStringMaxLen256
csfWorkspaceSecurityGroupId = Lens.field @"workspaceSecurityGroupId"
{-# DEPRECATED csfWorkspaceSecurityGroupId "Use generic-lens or generic-optics with 'workspaceSecurityGroupId' instead." #-}

-- | The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
--
-- /Note:/ Consider using 'engineSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfEngineSecurityGroupId :: Lens.Lens' CreateStudio Types.XmlStringMaxLen256
csfEngineSecurityGroupId = Lens.field @"engineSecurityGroupId"
{-# DEPRECATED csfEngineSecurityGroupId "Use generic-lens or generic-optics with 'engineSecurityGroupId' instead." #-}

-- | The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
--
-- /Note:/ Consider using 'defaultS3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDefaultS3Location :: Lens.Lens' CreateStudio (Core.Maybe Types.XmlString)
csfDefaultS3Location = Lens.field @"defaultS3Location"
{-# DEPRECATED csfDefaultS3Location "Use generic-lens or generic-optics with 'defaultS3Location' instead." #-}

-- | A detailed description of the Studio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDescription :: Lens.Lens' CreateStudio (Core.Maybe Types.XmlStringMaxLen256)
csfDescription = Lens.field @"description"
{-# DEPRECATED csfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfTags :: Lens.Lens' CreateStudio (Core.Maybe [Types.Tag])
csfTags = Lens.field @"tags"
{-# DEPRECATED csfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateStudio where
  toJSON CreateStudio {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("AuthMode" Core..= authMode),
            Core.Just ("VpcId" Core..= vpcId),
            Core.Just ("SubnetIds" Core..= subnetIds),
            Core.Just ("ServiceRole" Core..= serviceRole),
            Core.Just ("UserRole" Core..= userRole),
            Core.Just
              ("WorkspaceSecurityGroupId" Core..= workspaceSecurityGroupId),
            Core.Just ("EngineSecurityGroupId" Core..= engineSecurityGroupId),
            ("DefaultS3Location" Core..=) Core.<$> defaultS3Location,
            ("Description" Core..=) Core.<$> description,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateStudio where
  type Rs CreateStudio = CreateStudioResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ElasticMapReduce.CreateStudio")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStudioResponse'
            Core.<$> (x Core..:? "StudioId")
            Core.<*> (x Core..:? "Url")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateStudioResponse' smart constructor.
data CreateStudioResponse = CreateStudioResponse'
  { -- | The ID of the Amazon EMR Studio.
    studioId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The unique Studio access URL.
    url :: Core.Maybe Types.XmlString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStudioResponse' value with any optional fields omitted.
mkCreateStudioResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateStudioResponse
mkCreateStudioResponse responseStatus =
  CreateStudioResponse'
    { studioId = Core.Nothing,
      url = Core.Nothing,
      responseStatus
    }

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsStudioId :: Lens.Lens' CreateStudioResponse (Core.Maybe Types.XmlStringMaxLen256)
csrrsStudioId = Lens.field @"studioId"
{-# DEPRECATED csrrsStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | The unique Studio access URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsUrl :: Lens.Lens' CreateStudioResponse (Core.Maybe Types.XmlString)
csrrsUrl = Lens.field @"url"
{-# DEPRECATED csrrsUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateStudioResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
