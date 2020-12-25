{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.PutFileSystemPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies an Amazon EFS @FileSystemPolicy@ to an Amazon EFS file system. A file system policy is an IAM resource-based policy and can contain multiple policy statements. A file system always has exactly one file system policy, which can be the default policy or an explicit policy set or updated using this API operation. When an explicit policy is set, it overrides the default policy. For more information about the default file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/iam-access-control-nfs-efs.html#default-filesystempolicy Default EFS File System Policy> .
--
-- This operation requires permissions for the @elasticfilesystem:PutFileSystemPolicy@ action.
module Network.AWS.EFS.PutFileSystemPolicy
  ( -- * Creating a request
    PutFileSystemPolicy (..),
    mkPutFileSystemPolicy,

    -- ** Request lenses
    pfspFileSystemId,
    pfspPolicy,
    pfspBypassPolicyLockoutSafetyCheck,

    -- * Destructuring the response
    Types.FileSystemPolicyDescription (..),
    Types.mkFileSystemPolicyDescription,

    -- ** Response lenses
    Types.fspdFileSystemId,
    Types.fspdPolicy,
  )
where

import qualified Network.AWS.EFS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutFileSystemPolicy' smart constructor.
data PutFileSystemPolicy = PutFileSystemPolicy'
  { -- | The ID of the EFS file system that you want to create or update the @FileSystemPolicy@ for.
    fileSystemId :: Types.FileSystemId,
    -- | The @FileSystemPolicy@ that you're creating. Accepts a JSON formatted policy definition. To find out more about the elements that make up a file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies> .
    policy :: Types.Policy,
    -- | (Optional) A flag to indicate whether to bypass the @FileSystemPolicy@ lockout safety check. The policy lockout safety check determines whether the policy in the request will prevent the principal making the request will be locked out from making future @PutFileSystemPolicy@ requests on the file system. Set @BypassPolicyLockoutSafetyCheck@ to @True@ only when you intend to prevent the principal that is making the request from making a subsequent @PutFileSystemPolicy@ request on the file system. The default value is False.
    bypassPolicyLockoutSafetyCheck :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutFileSystemPolicy' value with any optional fields omitted.
mkPutFileSystemPolicy ::
  -- | 'fileSystemId'
  Types.FileSystemId ->
  -- | 'policy'
  Types.Policy ->
  PutFileSystemPolicy
mkPutFileSystemPolicy fileSystemId policy =
  PutFileSystemPolicy'
    { fileSystemId,
      policy,
      bypassPolicyLockoutSafetyCheck = Core.Nothing
    }

-- | The ID of the EFS file system that you want to create or update the @FileSystemPolicy@ for.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfspFileSystemId :: Lens.Lens' PutFileSystemPolicy Types.FileSystemId
pfspFileSystemId = Lens.field @"fileSystemId"
{-# DEPRECATED pfspFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The @FileSystemPolicy@ that you're creating. Accepts a JSON formatted policy definition. To find out more about the elements that make up a file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies> .
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfspPolicy :: Lens.Lens' PutFileSystemPolicy Types.Policy
pfspPolicy = Lens.field @"policy"
{-# DEPRECATED pfspPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | (Optional) A flag to indicate whether to bypass the @FileSystemPolicy@ lockout safety check. The policy lockout safety check determines whether the policy in the request will prevent the principal making the request will be locked out from making future @PutFileSystemPolicy@ requests on the file system. Set @BypassPolicyLockoutSafetyCheck@ to @True@ only when you intend to prevent the principal that is making the request from making a subsequent @PutFileSystemPolicy@ request on the file system. The default value is False.
--
-- /Note:/ Consider using 'bypassPolicyLockoutSafetyCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfspBypassPolicyLockoutSafetyCheck :: Lens.Lens' PutFileSystemPolicy (Core.Maybe Core.Bool)
pfspBypassPolicyLockoutSafetyCheck = Lens.field @"bypassPolicyLockoutSafetyCheck"
{-# DEPRECATED pfspBypassPolicyLockoutSafetyCheck "Use generic-lens or generic-optics with 'bypassPolicyLockoutSafetyCheck' instead." #-}

instance Core.FromJSON PutFileSystemPolicy where
  toJSON PutFileSystemPolicy {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Policy" Core..= policy),
            ("BypassPolicyLockoutSafetyCheck" Core..=)
              Core.<$> bypassPolicyLockoutSafetyCheck
          ]
      )

instance Core.AWSRequest PutFileSystemPolicy where
  type Rs PutFileSystemPolicy = Types.FileSystemPolicyDescription
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/2015-02-01/file-systems/" Core.<> (Core.toText fileSystemId)
                Core.<> ("/policy")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
