{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessKeyMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AccessKeyMetadata
  ( AccessKeyMetadata (..),

    -- * Smart constructor
    mkAccessKeyMetadata,

    -- * Lenses
    akmAccessKeyId,
    akmCreateDate,
    akmStatus,
    akmUserName,
  )
where

import qualified Network.AWS.IAM.Types.StatusType as Types
import qualified Network.AWS.IAM.Types.UserNameType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an AWS access key, without its secret key.
--
-- This data type is used as a response element in the 'ListAccessKeys' operation.
--
-- /See:/ 'mkAccessKeyMetadata' smart constructor.
data AccessKeyMetadata = AccessKeyMetadata'
  { -- | The ID for this access key.
    accessKeyId :: Core.Maybe Types.AccessKey,
    -- | The date when the access key was created.
    createDate :: Core.Maybe Core.UTCTime,
    -- | The status of the access key. @Active@ means that the key is valid for API calls; @Inactive@ means it is not.
    status :: Core.Maybe Types.StatusType,
    -- | The name of the IAM user that the key is associated with.
    userName :: Core.Maybe Types.UserNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AccessKeyMetadata' value with any optional fields omitted.
mkAccessKeyMetadata ::
  AccessKeyMetadata
mkAccessKeyMetadata =
  AccessKeyMetadata'
    { accessKeyId = Core.Nothing,
      createDate = Core.Nothing,
      status = Core.Nothing,
      userName = Core.Nothing
    }

-- | The ID for this access key.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akmAccessKeyId :: Lens.Lens' AccessKeyMetadata (Core.Maybe Types.AccessKey)
akmAccessKeyId = Lens.field @"accessKeyId"
{-# DEPRECATED akmAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

-- | The date when the access key was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akmCreateDate :: Lens.Lens' AccessKeyMetadata (Core.Maybe Core.UTCTime)
akmCreateDate = Lens.field @"createDate"
{-# DEPRECATED akmCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The status of the access key. @Active@ means that the key is valid for API calls; @Inactive@ means it is not.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akmStatus :: Lens.Lens' AccessKeyMetadata (Core.Maybe Types.StatusType)
akmStatus = Lens.field @"status"
{-# DEPRECATED akmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the IAM user that the key is associated with.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akmUserName :: Lens.Lens' AccessKeyMetadata (Core.Maybe Types.UserNameType)
akmUserName = Lens.field @"userName"
{-# DEPRECATED akmUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.FromXML AccessKeyMetadata where
  parseXML x =
    AccessKeyMetadata'
      Core.<$> (x Core..@? "AccessKeyId")
      Core.<*> (x Core..@? "CreateDate")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "UserName")
