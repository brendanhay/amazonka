{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateInstanceProfile (..),
    mkCreateInstanceProfile,

    -- ** Request lenses
    cipInstanceProfileName,
    cipPath,

    -- * Destructuring the response
    CreateInstanceProfileResponse (..),
    mkCreateInstanceProfileResponse,

    -- ** Response lenses
    ciprrsInstanceProfile,
    ciprrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInstanceProfile' smart constructor.
data CreateInstanceProfile = CreateInstanceProfile'
  { -- | The name of the instance profile to create.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    instanceProfileName :: Types.InstanceProfileName,
    -- | The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    --
    -- This parameter is optional. If it is not included, it defaults to a slash (/).
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    path :: Core.Maybe Types.Path
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceProfile' value with any optional fields omitted.
mkCreateInstanceProfile ::
  -- | 'instanceProfileName'
  Types.InstanceProfileName ->
  CreateInstanceProfile
mkCreateInstanceProfile instanceProfileName =
  CreateInstanceProfile' {instanceProfileName, path = Core.Nothing}

-- | The name of the instance profile to create.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipInstanceProfileName :: Lens.Lens' CreateInstanceProfile Types.InstanceProfileName
cipInstanceProfileName = Lens.field @"instanceProfileName"
{-# DEPRECATED cipInstanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead." #-}

-- | The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipPath :: Lens.Lens' CreateInstanceProfile (Core.Maybe Types.Path)
cipPath = Lens.field @"path"
{-# DEPRECATED cipPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Core.AWSRequest CreateInstanceProfile where
  type Rs CreateInstanceProfile = CreateInstanceProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateInstanceProfile")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "InstanceProfileName" instanceProfileName)
                Core.<> (Core.toQueryValue "Path" Core.<$> path)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateInstanceProfileResult"
      ( \s h x ->
          CreateInstanceProfileResponse'
            Core.<$> (x Core..@ "InstanceProfile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a successful 'CreateInstanceProfile' request.
--
-- /See:/ 'mkCreateInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
  { -- | A structure containing details about the new instance profile.
    instanceProfile :: Types.InstanceProfile,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateInstanceProfileResponse' value with any optional fields omitted.
mkCreateInstanceProfileResponse ::
  -- | 'instanceProfile'
  Types.InstanceProfile ->
  -- | 'responseStatus'
  Core.Int ->
  CreateInstanceProfileResponse
mkCreateInstanceProfileResponse instanceProfile responseStatus =
  CreateInstanceProfileResponse' {instanceProfile, responseStatus}

-- | A structure containing details about the new instance profile.
--
-- /Note:/ Consider using 'instanceProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprrsInstanceProfile :: Lens.Lens' CreateInstanceProfileResponse Types.InstanceProfile
ciprrsInstanceProfile = Lens.field @"instanceProfile"
{-# DEPRECATED ciprrsInstanceProfile "Use generic-lens or generic-optics with 'instanceProfile' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciprrsResponseStatus :: Lens.Lens' CreateInstanceProfileResponse Core.Int
ciprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ciprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
