{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified instance profile. The instance profile must not have an associated role.
--
-- /Important:/ Make sure that you do not have any Amazon EC2 instances running with the instance profile you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.
-- For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
module Network.AWS.IAM.DeleteInstanceProfile
    (
    -- * Creating a request
      DeleteInstanceProfile (..)
    , mkDeleteInstanceProfile
    -- ** Request lenses
    , dipInstanceProfileName

    -- * Destructuring the response
    , DeleteInstanceProfileResponse (..)
    , mkDeleteInstanceProfileResponse
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInstanceProfile' smart constructor.
newtype DeleteInstanceProfile = DeleteInstanceProfile'
  { instanceProfileName :: Types.InstanceProfileName
    -- ^ The name of the instance profile to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstanceProfile' value with any optional fields omitted.
mkDeleteInstanceProfile
    :: Types.InstanceProfileName -- ^ 'instanceProfileName'
    -> DeleteInstanceProfile
mkDeleteInstanceProfile instanceProfileName
  = DeleteInstanceProfile'{instanceProfileName}

-- | The name of the instance profile to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipInstanceProfileName :: Lens.Lens' DeleteInstanceProfile Types.InstanceProfileName
dipInstanceProfileName = Lens.field @"instanceProfileName"
{-# INLINEABLE dipInstanceProfileName #-}
{-# DEPRECATED instanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead"  #-}

instance Core.ToQuery DeleteInstanceProfile where
        toQuery DeleteInstanceProfile{..}
          = Core.toQueryPair "Action" ("DeleteInstanceProfile" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceProfileName" instanceProfileName

instance Core.ToHeaders DeleteInstanceProfile where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteInstanceProfile where
        type Rs DeleteInstanceProfile = DeleteInstanceProfileResponse
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
        parseResponse = Response.receiveNull DeleteInstanceProfileResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteInstanceProfileResponse' smart constructor.
data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInstanceProfileResponse' value with any optional fields omitted.
mkDeleteInstanceProfileResponse
    :: DeleteInstanceProfileResponse
mkDeleteInstanceProfileResponse = DeleteInstanceProfileResponse'
