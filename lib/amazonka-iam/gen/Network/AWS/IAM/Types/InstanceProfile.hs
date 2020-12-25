{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.InstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.InstanceProfile
  ( InstanceProfile (..),

    -- * Smart constructor
    mkInstanceProfile,

    -- * Lenses
    ipPath,
    ipInstanceProfileName,
    ipInstanceProfileId,
    ipArn,
    ipCreateDate,
    ipRoles,
  )
where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.IAM.Types.InstanceProfileId as Types
import qualified Network.AWS.IAM.Types.InstanceProfileName as Types
import qualified Network.AWS.IAM.Types.Path as Types
import qualified Network.AWS.IAM.Types.Role as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an instance profile.
--
-- This data type is used as a response element in the following operations:
--
--     * 'CreateInstanceProfile'
--
--
--     * 'GetInstanceProfile'
--
--
--     * 'ListInstanceProfiles'
--
--
--     * 'ListInstanceProfilesForRole'
--
--
--
-- /See:/ 'mkInstanceProfile' smart constructor.
data InstanceProfile = InstanceProfile'
  { -- | The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    path :: Types.Path,
    -- | The name identifying the instance profile.
    instanceProfileName :: Types.InstanceProfileName,
    -- | The stable and unique string identifying the instance profile. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    instanceProfileId :: Types.InstanceProfileId,
    -- | The Amazon Resource Name (ARN) specifying the instance profile. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
    arn :: Types.Arn,
    -- | The date when the instance profile was created.
    createDate :: Core.UTCTime,
    -- | The role associated with the instance profile.
    roles :: [Types.Role]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InstanceProfile' value with any optional fields omitted.
mkInstanceProfile ::
  -- | 'path'
  Types.Path ->
  -- | 'instanceProfileName'
  Types.InstanceProfileName ->
  -- | 'instanceProfileId'
  Types.InstanceProfileId ->
  -- | 'arn'
  Types.Arn ->
  -- | 'createDate'
  Core.UTCTime ->
  InstanceProfile
mkInstanceProfile
  path
  instanceProfileName
  instanceProfileId
  arn
  createDate =
    InstanceProfile'
      { path,
        instanceProfileName,
        instanceProfileId,
        arn,
        createDate,
        roles = Core.mempty
      }

-- | The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipPath :: Lens.Lens' InstanceProfile Types.Path
ipPath = Lens.field @"path"
{-# DEPRECATED ipPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name identifying the instance profile.
--
-- /Note:/ Consider using 'instanceProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipInstanceProfileName :: Lens.Lens' InstanceProfile Types.InstanceProfileName
ipInstanceProfileName = Lens.field @"instanceProfileName"
{-# DEPRECATED ipInstanceProfileName "Use generic-lens or generic-optics with 'instanceProfileName' instead." #-}

-- | The stable and unique string identifying the instance profile. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'instanceProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipInstanceProfileId :: Lens.Lens' InstanceProfile Types.InstanceProfileId
ipInstanceProfileId = Lens.field @"instanceProfileId"
{-# DEPRECATED ipInstanceProfileId "Use generic-lens or generic-optics with 'instanceProfileId' instead." #-}

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipArn :: Lens.Lens' InstanceProfile Types.Arn
ipArn = Lens.field @"arn"
{-# DEPRECATED ipArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the instance profile was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipCreateDate :: Lens.Lens' InstanceProfile Core.UTCTime
ipCreateDate = Lens.field @"createDate"
{-# DEPRECATED ipCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The role associated with the instance profile.
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipRoles :: Lens.Lens' InstanceProfile [Types.Role]
ipRoles = Lens.field @"roles"
{-# DEPRECATED ipRoles "Use generic-lens or generic-optics with 'roles' instead." #-}

instance Core.FromXML InstanceProfile where
  parseXML x =
    InstanceProfile'
      Core.<$> (x Core..@ "Path")
      Core.<*> (x Core..@ "InstanceProfileName")
      Core.<*> (x Core..@ "InstanceProfileId")
      Core.<*> (x Core..@ "Arn")
      Core.<*> (x Core..@ "CreateDate")
      Core.<*> ( x Core..@? "Roles" Core..@! Core.mempty
                   Core..<@> Core.parseXMLList "member"
               )
