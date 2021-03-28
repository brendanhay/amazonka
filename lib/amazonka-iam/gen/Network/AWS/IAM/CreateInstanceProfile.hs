{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new instance profile. For information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
--
-- The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateInstanceProfile
    (
    -- * Creating a request
      CreateInstanceProfile (..)
    , mkCreateInstanceProfile
    -- ** Request lenses
    , cipInstanceProfileName
    , cipPath

    -- * Destructuring the response
    , CreateInstanceProfileResponse (..)
    , mkCreateInstanceProfileResponse
    -- ** Response lenses
    , ciprrsInstanceProfile
    , ciprrsResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInstanceProfile' smart constructor.
data CreateInstanceProfile = CreateInstanceProfile'
  { instanceProfileName :: Types.InstanceProfileName
    -- ^ The name of the instance profile to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , path :: Core.Maybe Types.Path
    -- ^ The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceProfile' value with any optional fields omitted.
mkCreateInstanceProfile
    :: Types.InstanceProfileName -- ^ 'instanceProfileName'
    -> CreateInstanceProfile
mkCreateInstanceProfile instanceProfileName
  = CreateInstanceProfile'{instanceProfileName, path = Core.Nothing}

-- | The name of the instance profile to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipInstanceProfileName :: Lens.Lens' CreateInstanceProfile Types.InstanceProfileName
cipInstanceProfileName = Lens.field @"instanceProfileName"
{-# INLINEABLE cipInstanceProfileName #-}
{-# DEPRECATED instanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead"  #-}

-- | The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipPath :: Lens.Lens' CreateInstanceProfile (Core.Maybe Types.Path)
cipPath = Lens.field @"path"
{-# INLINEABLE cipPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.ToQuery CreateInstanceProfile where
        toQuery CreateInstanceProfile{..}
          = Core.toQueryPair "Action" ("CreateInstanceProfile" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceProfileName" instanceProfileName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Path") path

instance Core.ToHeaders CreateInstanceProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateInstanceProfile where
        type Rs CreateInstanceProfile = CreateInstanceProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateInstanceProfileResult"
              (\ s h x ->
                 CreateInstanceProfileResponse' Core.<$>
                   (x Core..@ "InstanceProfile") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'CreateInstanceProfile' request. 
--
-- /See:/ 'mkCreateInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
  { instanceProfile :: Types.InstanceProfile
    -- ^ A structure containing details about the new instance profile.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateInstanceProfileResponse' value with any optional fields omitted.
mkCreateInstanceProfileResponse
    :: Types.InstanceProfile -- ^ 'instanceProfile'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateInstanceProfileResponse
mkCreateInstanceProfileResponse instanceProfile responseStatus
  = CreateInstanceProfileResponse'{instanceProfile, responseStatus}

-- | A structure containing details about the new instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprrsInstanceProfile :: Lens.Lens' CreateInstanceProfileResponse Types.InstanceProfile
ciprrsInstanceProfile = Lens.field @"instanceProfile"
{-# INLINEABLE ciprrsInstanceProfile #-}
{-# DEPRECATED instanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprrsResponseStatus :: Lens.Lens' CreateInstanceProfileResponse Core.Int
ciprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ciprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
