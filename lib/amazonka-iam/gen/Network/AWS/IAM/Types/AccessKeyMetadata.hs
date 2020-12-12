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
    akmStatus,
    akmCreateDate,
    akmUserName,
    akmAccessKeyId,
  )
where

import Network.AWS.IAM.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an AWS access key, without its secret key.
--
-- This data type is used as a response element in the 'ListAccessKeys' operation.
--
-- /See:/ 'mkAccessKeyMetadata' smart constructor.
data AccessKeyMetadata = AccessKeyMetadata'
  { status ::
      Lude.Maybe StatusType,
    createDate :: Lude.Maybe Lude.DateTime,
    userName :: Lude.Maybe Lude.Text,
    accessKeyId :: Lude.Maybe AccessKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessKeyMetadata' with the minimum fields required to make a request.
--
-- * 'accessKeyId' - The ID for this access key.
-- * 'createDate' - The date when the access key was created.
-- * 'status' - The status of the access key. @Active@ means that the key is valid for API calls; @Inactive@ means it is not.
-- * 'userName' - The name of the IAM user that the key is associated with.
mkAccessKeyMetadata ::
  AccessKeyMetadata
mkAccessKeyMetadata =
  AccessKeyMetadata'
    { status = Lude.Nothing,
      createDate = Lude.Nothing,
      userName = Lude.Nothing,
      accessKeyId = Lude.Nothing
    }

-- | The status of the access key. @Active@ means that the key is valid for API calls; @Inactive@ means it is not.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akmStatus :: Lens.Lens' AccessKeyMetadata (Lude.Maybe StatusType)
akmStatus = Lens.lens (status :: AccessKeyMetadata -> Lude.Maybe StatusType) (\s a -> s {status = a} :: AccessKeyMetadata)
{-# DEPRECATED akmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date when the access key was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akmCreateDate :: Lens.Lens' AccessKeyMetadata (Lude.Maybe Lude.DateTime)
akmCreateDate = Lens.lens (createDate :: AccessKeyMetadata -> Lude.Maybe Lude.DateTime) (\s a -> s {createDate = a} :: AccessKeyMetadata)
{-# DEPRECATED akmCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The name of the IAM user that the key is associated with.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akmUserName :: Lens.Lens' AccessKeyMetadata (Lude.Maybe Lude.Text)
akmUserName = Lens.lens (userName :: AccessKeyMetadata -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: AccessKeyMetadata)
{-# DEPRECATED akmUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The ID for this access key.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akmAccessKeyId :: Lens.Lens' AccessKeyMetadata (Lude.Maybe AccessKey)
akmAccessKeyId = Lens.lens (accessKeyId :: AccessKeyMetadata -> Lude.Maybe AccessKey) (\s a -> s {accessKeyId = a} :: AccessKeyMetadata)
{-# DEPRECATED akmAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

instance Lude.FromXML AccessKeyMetadata where
  parseXML x =
    AccessKeyMetadata'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "CreateDate")
      Lude.<*> (x Lude..@? "UserName")
      Lude.<*> (x Lude..@? "AccessKeyId")
