{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Simple AD directory. For more information, see <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_simple_ad.html Simple Active Directory> in the /AWS Directory Service Admin Guide/ .
--
-- Before you call @CreateDirectory@ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the @CreateDirectory@ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
module Network.AWS.DirectoryService.CreateDirectory
    (
    -- * Creating a request
      CreateDirectory (..)
    , mkCreateDirectory
    -- ** Request lenses
    , cName
    , cPassword
    , cSize
    , cDescription
    , cShortName
    , cTags
    , cVpcSettings

    -- * Destructuring the response
    , CreateDirectoryResponse (..)
    , mkCreateDirectoryResponse
    -- ** Response lenses
    , crsDirectoryId
    , crsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the 'CreateDirectory' operation. 
--
-- /See:/ 'mkCreateDirectory' smart constructor.
data CreateDirectory = CreateDirectory'
  { name :: Types.Name
    -- ^ The fully qualified name for the directory, such as @corp.example.com@ .
  , password :: Types.Password
    -- ^ The password for the directory administrator. The directory creation process creates a directory administrator account with the user name @Administrator@ and this password.
--
-- If you need to change the password for the administrator account, you can use the 'ResetUserPassword' API call.
-- The regex pattern for this string is made up of the following conditions:
--
--     * Length (?=^.{8,64}$) – Must be between 8 and 64 characters
--
--
-- AND any 3 of the following password complexity rules required by Active Directory:
--
--     * Numbers and upper case and lowercase (?=.*\d)(?=.*[A-Z])(?=.*[a-z])
--
--
--     * Numbers and special characters and lower case (?=.*\d)(?=.*[^A-Za-z0-9\s])(?=.*[a-z])
--
--
--     * Special characters and upper case and lower case (?=.*[^A-Za-z0-9\s])(?=.*[A-Z])(?=.*[a-z])
--
--
--     * Numbers and upper case and special characters (?=.*\d)(?=.*[A-Z])(?=.*[^A-Za-z0-9\s])
--
--
-- For additional information about how Active Directory passwords are enforced, see <https://docs.microsoft.com/en-us/windows/security/threat-protection/security-policy-settings/password-must-meet-complexity-requirements Password must meet complexity requirements> on the Microsoft website.
  , size :: Types.DirectorySize
    -- ^ The size of the directory.
  , description :: Core.Maybe Types.Description
    -- ^ A description for the directory.
  , shortName :: Core.Maybe Types.DirectoryShortName
    -- ^ The NetBIOS name of the directory, such as @CORP@ .
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to be assigned to the Simple AD directory.
  , vpcSettings :: Core.Maybe Types.DirectoryVpcSettings
    -- ^ A 'DirectoryVpcSettings' object that contains additional information for the operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectory' value with any optional fields omitted.
mkCreateDirectory
    :: Types.Name -- ^ 'name'
    -> Types.Password -- ^ 'password'
    -> Types.DirectorySize -- ^ 'size'
    -> CreateDirectory
mkCreateDirectory name password size
  = CreateDirectory'{name, password, size,
                     description = Core.Nothing, shortName = Core.Nothing,
                     tags = Core.Nothing, vpcSettings = Core.Nothing}

-- | The fully qualified name for the directory, such as @corp.example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' CreateDirectory Types.Name
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The password for the directory administrator. The directory creation process creates a directory administrator account with the user name @Administrator@ and this password.
--
-- If you need to change the password for the administrator account, you can use the 'ResetUserPassword' API call.
-- The regex pattern for this string is made up of the following conditions:
--
--     * Length (?=^.{8,64}$) – Must be between 8 and 64 characters
--
--
-- AND any 3 of the following password complexity rules required by Active Directory:
--
--     * Numbers and upper case and lowercase (?=.*\d)(?=.*[A-Z])(?=.*[a-z])
--
--
--     * Numbers and special characters and lower case (?=.*\d)(?=.*[^A-Za-z0-9\s])(?=.*[a-z])
--
--
--     * Special characters and upper case and lower case (?=.*[^A-Za-z0-9\s])(?=.*[A-Z])(?=.*[a-z])
--
--
--     * Numbers and upper case and special characters (?=.*\d)(?=.*[A-Z])(?=.*[^A-Za-z0-9\s])
--
--
-- For additional information about how Active Directory passwords are enforced, see <https://docs.microsoft.com/en-us/windows/security/threat-protection/security-policy-settings/password-must-meet-complexity-requirements Password must meet complexity requirements> on the Microsoft website.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cPassword :: Lens.Lens' CreateDirectory Types.Password
cPassword = Lens.field @"password"
{-# INLINEABLE cPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The size of the directory.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSize :: Lens.Lens' CreateDirectory Types.DirectorySize
cSize = Lens.field @"size"
{-# INLINEABLE cSize #-}
{-# DEPRECATED size "Use generic-lens or generic-optics with 'size' instead"  #-}

-- | A description for the directory.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' CreateDirectory (Core.Maybe Types.Description)
cDescription = Lens.field @"description"
{-# INLINEABLE cDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The NetBIOS name of the directory, such as @CORP@ .
--
-- /Note:/ Consider using 'shortName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cShortName :: Lens.Lens' CreateDirectory (Core.Maybe Types.DirectoryShortName)
cShortName = Lens.field @"shortName"
{-# INLINEABLE cShortName #-}
{-# DEPRECATED shortName "Use generic-lens or generic-optics with 'shortName' instead"  #-}

-- | The tags to be assigned to the Simple AD directory.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateDirectory (Core.Maybe [Types.Tag])
cTags = Lens.field @"tags"
{-# INLINEABLE cTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A 'DirectoryVpcSettings' object that contains additional information for the operation.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVpcSettings :: Lens.Lens' CreateDirectory (Core.Maybe Types.DirectoryVpcSettings)
cVpcSettings = Lens.field @"vpcSettings"
{-# INLINEABLE cVpcSettings #-}
{-# DEPRECATED vpcSettings "Use generic-lens or generic-optics with 'vpcSettings' instead"  #-}

instance Core.ToQuery CreateDirectory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDirectory where
        toHeaders CreateDirectory{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.CreateDirectory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateDirectory where
        toJSON CreateDirectory{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Password" Core..= password),
                  Core.Just ("Size" Core..= size),
                  ("Description" Core..=) Core.<$> description,
                  ("ShortName" Core..=) Core.<$> shortName,
                  ("Tags" Core..=) Core.<$> tags,
                  ("VpcSettings" Core..=) Core.<$> vpcSettings])

instance Core.AWSRequest CreateDirectory where
        type Rs CreateDirectory = CreateDirectoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateDirectoryResponse' Core.<$>
                   (x Core..:? "DirectoryId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the results of the 'CreateDirectory' operation.
--
-- /See:/ 'mkCreateDirectoryResponse' smart constructor.
data CreateDirectoryResponse = CreateDirectoryResponse'
  { directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The identifier of the directory that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDirectoryResponse' value with any optional fields omitted.
mkCreateDirectoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDirectoryResponse
mkCreateDirectoryResponse responseStatus
  = CreateDirectoryResponse'{directoryId = Core.Nothing,
                             responseStatus}

-- | The identifier of the directory that was created.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsDirectoryId :: Lens.Lens' CreateDirectoryResponse (Core.Maybe Types.DirectoryId)
crsDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE crsDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateDirectoryResponse Core.Int
crsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
