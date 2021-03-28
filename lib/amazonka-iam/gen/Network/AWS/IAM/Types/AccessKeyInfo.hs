{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessKeyInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.AccessKeyInfo
  ( AccessKeyInfo (..)
  -- * Smart constructor
  , mkAccessKeyInfo
  -- * Lenses
  , akiUserName
  , akiAccessKeyId
  , akiStatus
  , akiSecretAccessKey
  , akiCreateDate
  ) where

import qualified Network.AWS.IAM.Types.AccessKeySecretType as Types
import qualified Network.AWS.IAM.Types.StatusType as Types
import qualified Network.AWS.IAM.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an AWS access key.
--
-- This data type is used as a response element in the 'CreateAccessKey' and 'ListAccessKeys' operations. 
--
-- /See:/ 'mkAccessKeyInfo' smart constructor.
data AccessKeyInfo = AccessKeyInfo'
  { userName :: Types.UserName
    -- ^ The name of the IAM user that the access key is associated with.
  , accessKeyId :: Types.AccessKey
    -- ^ The ID for this access key.
  , status :: Types.StatusType
    -- ^ The status of the access key. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not. 
  , secretAccessKey :: Types.AccessKeySecretType
    -- ^ The secret key used to sign requests.
  , createDate :: Core.Maybe Core.UTCTime
    -- ^ The date when the access key was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AccessKeyInfo' value with any optional fields omitted.
mkAccessKeyInfo
    :: Types.UserName -- ^ 'userName'
    -> Types.AccessKey -- ^ 'accessKeyId'
    -> Types.StatusType -- ^ 'status'
    -> Types.AccessKeySecretType -- ^ 'secretAccessKey'
    -> AccessKeyInfo
mkAccessKeyInfo userName accessKeyId status secretAccessKey
  = AccessKeyInfo'{userName, accessKeyId, status, secretAccessKey,
                   createDate = Core.Nothing}

-- | The name of the IAM user that the access key is associated with.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akiUserName :: Lens.Lens' AccessKeyInfo Types.UserName
akiUserName = Lens.field @"userName"
{-# INLINEABLE akiUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

-- | The ID for this access key.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akiAccessKeyId :: Lens.Lens' AccessKeyInfo Types.AccessKey
akiAccessKeyId = Lens.field @"accessKeyId"
{-# INLINEABLE akiAccessKeyId #-}
{-# DEPRECATED accessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead"  #-}

-- | The status of the access key. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akiStatus :: Lens.Lens' AccessKeyInfo Types.StatusType
akiStatus = Lens.field @"status"
{-# INLINEABLE akiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The secret key used to sign requests.
--
-- /Note:/ Consider using 'secretAccessKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akiSecretAccessKey :: Lens.Lens' AccessKeyInfo Types.AccessKeySecretType
akiSecretAccessKey = Lens.field @"secretAccessKey"
{-# INLINEABLE akiSecretAccessKey #-}
{-# DEPRECATED secretAccessKey "Use generic-lens or generic-optics with 'secretAccessKey' instead"  #-}

-- | The date when the access key was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akiCreateDate :: Lens.Lens' AccessKeyInfo (Core.Maybe Core.UTCTime)
akiCreateDate = Lens.field @"createDate"
{-# INLINEABLE akiCreateDate #-}
{-# DEPRECATED createDate "Use generic-lens or generic-optics with 'createDate' instead"  #-}

instance Core.FromXML AccessKeyInfo where
        parseXML x
          = AccessKeyInfo' Core.<$>
              (x Core..@ "UserName") Core.<*> x Core..@ "AccessKeyId" Core.<*>
                x Core..@ "Status"
                Core.<*> x Core..@ "SecretAccessKey"
                Core.<*> x Core..@? "CreateDate"
