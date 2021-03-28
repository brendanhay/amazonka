{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.Group
  ( Group (..)
  -- * Smart constructor
  , mkGroup
  -- * Lenses
  , gPath
  , gGroupName
  , gGroupId
  , gArn
  , gCreateDate
  ) where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.IAM.Types.GroupId as Types
import qualified Network.AWS.IAM.Types.GroupName as Types
import qualified Network.AWS.IAM.Types.Path as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an IAM group entity.
--
-- This data type is used as a response element in the following operations:
--
--     * 'CreateGroup' 
--
--
--     * 'GetGroup' 
--
--
--     * 'ListGroups' 
--
--
--
-- /See:/ 'mkGroup' smart constructor.
data Group = Group'
  { path :: Types.Path
    -- ^ The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
  , groupName :: Types.GroupName
    -- ^ The friendly name that identifies the group.
  , groupId :: Types.GroupId
    -- ^ The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
  , arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) specifying the group. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
  , createDate :: Core.UTCTime
    -- ^ The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Group' value with any optional fields omitted.
mkGroup
    :: Types.Path -- ^ 'path'
    -> Types.GroupName -- ^ 'groupName'
    -> Types.GroupId -- ^ 'groupId'
    -> Types.Arn -- ^ 'arn'
    -> Core.UTCTime -- ^ 'createDate'
    -> Group
mkGroup path groupName groupId arn createDate
  = Group'{path, groupName, groupId, arn, createDate}

-- | The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPath :: Lens.Lens' Group Types.Path
gPath = Lens.field @"path"
{-# INLINEABLE gPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The friendly name that identifies the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGroupName :: Lens.Lens' Group Types.GroupName
gGroupName = Lens.field @"groupName"
{-# INLINEABLE gGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGroupId :: Lens.Lens' Group Types.GroupId
gGroupId = Lens.field @"groupId"
{-# INLINEABLE gGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The Amazon Resource Name (ARN) specifying the group. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gArn :: Lens.Lens' Group Types.Arn
gArn = Lens.field @"arn"
{-# INLINEABLE gArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gCreateDate :: Lens.Lens' Group Core.UTCTime
gCreateDate = Lens.field @"createDate"
{-# INLINEABLE gCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

instance Core.FromXML Group where
        parseXML x
          = Group' Core.<$>
              (x Core..@ "Path") Core.<*> x Core..@ "GroupName" Core.<*>
                x Core..@ "GroupId"
                Core.<*> x Core..@ "Arn"
                Core.<*> x Core..@ "CreateDate"
