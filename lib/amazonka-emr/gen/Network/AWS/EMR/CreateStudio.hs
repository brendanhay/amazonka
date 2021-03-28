{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateStudio (..)
    , mkCreateStudio
    -- ** Request lenses
    , csfName
    , csfAuthMode
    , csfVpcId
    , csfSubnetIds
    , csfServiceRole
    , csfUserRole
    , csfWorkspaceSecurityGroupId
    , csfEngineSecurityGroupId
    , csfDefaultS3Location
    , csfDescription
    , csfTags

    -- * Destructuring the response
    , CreateStudioResponse (..)
    , mkCreateStudioResponse
    -- ** Response lenses
    , csrrsStudioId
    , csrrsUrl
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateStudio' smart constructor.
data CreateStudio = CreateStudio'
  { name :: Types.XmlStringMaxLen256
    -- ^ A descriptive name for the Amazon EMR Studio.
  , authMode :: Types.AuthMode
    -- ^ Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
  , vpcId :: Types.XmlStringMaxLen256
    -- ^ The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
  , subnetIds :: [Core.Text]
    -- ^ A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
  , serviceRole :: Types.XmlString
    -- ^ The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
  , userRole :: Types.XmlString
    -- ^ The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
  , workspaceSecurityGroupId :: Types.XmlStringMaxLen256
    -- ^ The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
  , engineSecurityGroupId :: Types.XmlStringMaxLen256
    -- ^ The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
  , defaultS3Location :: Core.Maybe Types.XmlString
    -- ^ The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
  , description :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ A detailed description of the Studio.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStudio' value with any optional fields omitted.
mkCreateStudio
    :: Types.XmlStringMaxLen256 -- ^ 'name'
    -> Types.AuthMode -- ^ 'authMode'
    -> Types.XmlStringMaxLen256 -- ^ 'vpcId'
    -> Types.XmlString -- ^ 'serviceRole'
    -> Types.XmlString -- ^ 'userRole'
    -> Types.XmlStringMaxLen256 -- ^ 'workspaceSecurityGroupId'
    -> Types.XmlStringMaxLen256 -- ^ 'engineSecurityGroupId'
    -> CreateStudio
mkCreateStudio name authMode vpcId serviceRole userRole
  workspaceSecurityGroupId engineSecurityGroupId
  = CreateStudio'{name, authMode, vpcId, subnetIds = Core.mempty,
                  serviceRole, userRole, workspaceSecurityGroupId,
                  engineSecurityGroupId, defaultS3Location = Core.Nothing,
                  description = Core.Nothing, tags = Core.Nothing}

-- | A descriptive name for the Amazon EMR Studio.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfName :: Lens.Lens' CreateStudio Types.XmlStringMaxLen256
csfName = Lens.field @"name"
{-# INLINEABLE csfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Specifies whether the Studio authenticates users using single sign-on (SSO) or IAM. Amazon EMR Studio currently only supports SSO authentication.
--
-- /Note:/ Consider using 'authMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfAuthMode :: Lens.Lens' CreateStudio Types.AuthMode
csfAuthMode = Lens.field @"authMode"
{-# INLINEABLE csfAuthMode #-}
{-# DEPRECATED authMode "Use generic-lens or generic-optics with 'authMode' instead"  #-}

-- | The ID of the Amazon Virtual Private Cloud (Amazon VPC) to associate with the Studio.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfVpcId :: Lens.Lens' CreateStudio Types.XmlStringMaxLen256
csfVpcId = Lens.field @"vpcId"
{-# INLINEABLE csfVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | A list of subnet IDs to associate with the Studio. The subnets must belong to the VPC specified by @VpcId@ . Studio users can create a Workspace in any of the specified subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfSubnetIds :: Lens.Lens' CreateStudio [Core.Text]
csfSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE csfSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The IAM role that will be assumed by the Amazon EMR Studio. The service role provides a way for Amazon EMR Studio to interoperate with other AWS services.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfServiceRole :: Lens.Lens' CreateStudio Types.XmlString
csfServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE csfServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | The IAM user role that will be assumed by users and groups logged in to a Studio. The permissions attached to this IAM role can be scoped down for each user or group using session policies.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfUserRole :: Lens.Lens' CreateStudio Types.XmlString
csfUserRole = Lens.field @"userRole"
{-# INLINEABLE csfUserRole #-}
{-# DEPRECATED userRole "Use generic-lens or generic-optics with 'userRole' instead"  #-}

-- | The ID of the Amazon EMR Studio Workspace security group. The Workspace security group allows outbound network traffic to resources in the Engine security group, and it must be in the same VPC specified by @VpcId@ .
--
-- /Note:/ Consider using 'workspaceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfWorkspaceSecurityGroupId :: Lens.Lens' CreateStudio Types.XmlStringMaxLen256
csfWorkspaceSecurityGroupId = Lens.field @"workspaceSecurityGroupId"
{-# INLINEABLE csfWorkspaceSecurityGroupId #-}
{-# DEPRECATED workspaceSecurityGroupId "Use generic-lens or generic-optics with 'workspaceSecurityGroupId' instead"  #-}

-- | The ID of the Amazon EMR Studio Engine security group. The Engine security group allows inbound network traffic from the Workspace security group, and it must be in the same VPC specified by @VpcId@ .
--
-- /Note:/ Consider using 'engineSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfEngineSecurityGroupId :: Lens.Lens' CreateStudio Types.XmlStringMaxLen256
csfEngineSecurityGroupId = Lens.field @"engineSecurityGroupId"
{-# INLINEABLE csfEngineSecurityGroupId #-}
{-# DEPRECATED engineSecurityGroupId "Use generic-lens or generic-optics with 'engineSecurityGroupId' instead"  #-}

-- | The default Amazon S3 location to back up EMR Studio Workspaces and notebook files. A Studio user can select an alternative Amazon S3 location when creating a Workspace.
--
-- /Note:/ Consider using 'defaultS3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDefaultS3Location :: Lens.Lens' CreateStudio (Core.Maybe Types.XmlString)
csfDefaultS3Location = Lens.field @"defaultS3Location"
{-# INLINEABLE csfDefaultS3Location #-}
{-# DEPRECATED defaultS3Location "Use generic-lens or generic-optics with 'defaultS3Location' instead"  #-}

-- | A detailed description of the Studio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfDescription :: Lens.Lens' CreateStudio (Core.Maybe Types.XmlStringMaxLen256)
csfDescription = Lens.field @"description"
{-# INLINEABLE csfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of tags to associate with the Studio. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters, and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfTags :: Lens.Lens' CreateStudio (Core.Maybe [Types.Tag])
csfTags = Lens.field @"tags"
{-# INLINEABLE csfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateStudio where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateStudio where
        toHeaders CreateStudio{..}
          = Core.pure ("X-Amz-Target", "ElasticMapReduce.CreateStudio")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateStudio where
        toJSON CreateStudio{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
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
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateStudio where
        type Rs CreateStudio = CreateStudioResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateStudioResponse' Core.<$>
                   (x Core..:? "StudioId") Core.<*> x Core..:? "Url" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateStudioResponse' smart constructor.
data CreateStudioResponse = CreateStudioResponse'
  { studioId :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The ID of the Amazon EMR Studio.
  , url :: Core.Maybe Types.XmlString
    -- ^ The unique Studio access URL.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateStudioResponse' value with any optional fields omitted.
mkCreateStudioResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateStudioResponse
mkCreateStudioResponse responseStatus
  = CreateStudioResponse'{studioId = Core.Nothing,
                          url = Core.Nothing, responseStatus}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsStudioId :: Lens.Lens' CreateStudioResponse (Core.Maybe Types.XmlStringMaxLen256)
csrrsStudioId = Lens.field @"studioId"
{-# INLINEABLE csrrsStudioId #-}
{-# DEPRECATED studioId "Use generic-lens or generic-optics with 'studioId' instead"  #-}

-- | The unique Studio access URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsUrl :: Lens.Lens' CreateStudioResponse (Core.Maybe Types.XmlString)
csrrsUrl = Lens.field @"url"
{-# INLINEABLE csrrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateStudioResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
