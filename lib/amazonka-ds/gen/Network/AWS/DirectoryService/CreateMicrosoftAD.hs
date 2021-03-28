{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateMicrosoftAD
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Microsoft AD directory in the AWS Cloud. For more information, see <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html AWS Managed Microsoft AD> in the /AWS Directory Service Admin Guide/ .
--
-- Before you call /CreateMicrosoftAD/ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the /CreateMicrosoftAD/ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
module Network.AWS.DirectoryService.CreateMicrosoftAD
    (
    -- * Creating a request
      CreateMicrosoftAD (..)
    , mkCreateMicrosoftAD
    -- ** Request lenses
    , cmadName
    , cmadPassword
    , cmadVpcSettings
    , cmadDescription
    , cmadEdition
    , cmadShortName
    , cmadTags

    -- * Destructuring the response
    , CreateMicrosoftADResponse (..)
    , mkCreateMicrosoftADResponse
    -- ** Response lenses
    , cmadrrsDirectoryId
    , cmadrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates an AWS Managed Microsoft AD directory.
--
-- /See:/ 'mkCreateMicrosoftAD' smart constructor.
data CreateMicrosoftAD = CreateMicrosoftAD'
  { name :: Types.Name
    -- ^ The fully qualified domain name for the AWS Managed Microsoft AD directory, such as @corp.example.com@ . This name will resolve inside your VPC only. It does not need to be publicly resolvable.
  , password :: Types.Password
    -- ^ The password for the default administrative user named @Admin@ .
--
-- If you need to change the password for the administrator account, you can use the 'ResetUserPassword' API call.
  , vpcSettings :: Types.DirectoryVpcSettings
    -- ^ Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
  , description :: Core.Maybe Types.Description
    -- ^ A description for the directory. This label will appear on the AWS console @Directory Details@ page after the directory is created.
  , edition :: Core.Maybe Types.DirectoryEdition
    -- ^ AWS Managed Microsoft AD is available in two editions: @Standard@ and @Enterprise@ . @Enterprise@ is the default.
  , shortName :: Core.Maybe Types.DirectoryShortName
    -- ^ The NetBIOS name for your domain, such as @CORP@ . If you don't specify a NetBIOS name, it will default to the first part of your directory DNS. For example, @CORP@ for the directory DNS @corp.example.com@ . 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to be assigned to the AWS Managed Microsoft AD directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMicrosoftAD' value with any optional fields omitted.
mkCreateMicrosoftAD
    :: Types.Name -- ^ 'name'
    -> Types.Password -- ^ 'password'
    -> Types.DirectoryVpcSettings -- ^ 'vpcSettings'
    -> CreateMicrosoftAD
mkCreateMicrosoftAD name password vpcSettings
  = CreateMicrosoftAD'{name, password, vpcSettings,
                       description = Core.Nothing, edition = Core.Nothing,
                       shortName = Core.Nothing, tags = Core.Nothing}

-- | The fully qualified domain name for the AWS Managed Microsoft AD directory, such as @corp.example.com@ . This name will resolve inside your VPC only. It does not need to be publicly resolvable.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadName :: Lens.Lens' CreateMicrosoftAD Types.Name
cmadName = Lens.field @"name"
{-# INLINEABLE cmadName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The password for the default administrative user named @Admin@ .
--
-- If you need to change the password for the administrator account, you can use the 'ResetUserPassword' API call.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadPassword :: Lens.Lens' CreateMicrosoftAD Types.Password
cmadPassword = Lens.field @"password"
{-# INLINEABLE cmadPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadVpcSettings :: Lens.Lens' CreateMicrosoftAD Types.DirectoryVpcSettings
cmadVpcSettings = Lens.field @"vpcSettings"
{-# INLINEABLE cmadVpcSettings #-}
{-# DEPRECATED vpcSettings "Use generic-lens or generic-optics with 'vpcSettings' instead"  #-}

-- | A description for the directory. This label will appear on the AWS console @Directory Details@ page after the directory is created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadDescription :: Lens.Lens' CreateMicrosoftAD (Core.Maybe Types.Description)
cmadDescription = Lens.field @"description"
{-# INLINEABLE cmadDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | AWS Managed Microsoft AD is available in two editions: @Standard@ and @Enterprise@ . @Enterprise@ is the default.
--
-- /Note:/ Consider using 'edition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadEdition :: Lens.Lens' CreateMicrosoftAD (Core.Maybe Types.DirectoryEdition)
cmadEdition = Lens.field @"edition"
{-# INLINEABLE cmadEdition #-}
{-# DEPRECATED edition "Use generic-lens or generic-optics with 'edition' instead"  #-}

-- | The NetBIOS name for your domain, such as @CORP@ . If you don't specify a NetBIOS name, it will default to the first part of your directory DNS. For example, @CORP@ for the directory DNS @corp.example.com@ . 
--
-- /Note:/ Consider using 'shortName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadShortName :: Lens.Lens' CreateMicrosoftAD (Core.Maybe Types.DirectoryShortName)
cmadShortName = Lens.field @"shortName"
{-# INLINEABLE cmadShortName #-}
{-# DEPRECATED shortName "Use generic-lens or generic-optics with 'shortName' instead"  #-}

-- | The tags to be assigned to the AWS Managed Microsoft AD directory.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadTags :: Lens.Lens' CreateMicrosoftAD (Core.Maybe [Types.Tag])
cmadTags = Lens.field @"tags"
{-# INLINEABLE cmadTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateMicrosoftAD where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateMicrosoftAD where
        toHeaders CreateMicrosoftAD{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.CreateMicrosoftAD")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateMicrosoftAD where
        toJSON CreateMicrosoftAD{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Password" Core..= password),
                  Core.Just ("VpcSettings" Core..= vpcSettings),
                  ("Description" Core..=) Core.<$> description,
                  ("Edition" Core..=) Core.<$> edition,
                  ("ShortName" Core..=) Core.<$> shortName,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateMicrosoftAD where
        type Rs CreateMicrosoftAD = CreateMicrosoftADResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateMicrosoftADResponse' Core.<$>
                   (x Core..:? "DirectoryId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Result of a CreateMicrosoftAD request.
--
-- /See:/ 'mkCreateMicrosoftADResponse' smart constructor.
data CreateMicrosoftADResponse = CreateMicrosoftADResponse'
  { directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The identifier of the directory that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMicrosoftADResponse' value with any optional fields omitted.
mkCreateMicrosoftADResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateMicrosoftADResponse
mkCreateMicrosoftADResponse responseStatus
  = CreateMicrosoftADResponse'{directoryId = Core.Nothing,
                               responseStatus}

-- | The identifier of the directory that was created.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadrrsDirectoryId :: Lens.Lens' CreateMicrosoftADResponse (Core.Maybe Types.DirectoryId)
cmadrrsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE cmadrrsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmadrrsResponseStatus :: Lens.Lens' CreateMicrosoftADResponse Core.Int
cmadrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmadrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
