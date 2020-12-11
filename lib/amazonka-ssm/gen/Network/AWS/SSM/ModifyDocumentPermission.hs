{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ModifyDocumentPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares a Systems Manager document publicly or privately. If you share a document privately, you must specify the AWS user account IDs for those people who can use the document. If you share a document publicly, you must specify /All/ as the account ID.
module Network.AWS.SSM.ModifyDocumentPermission
  ( -- * Creating a request
    ModifyDocumentPermission (..),
    mkModifyDocumentPermission,

    -- ** Request lenses
    mdpSharedDocumentVersion,
    mdpAccountIdsToAdd,
    mdpAccountIdsToRemove,
    mdpName,
    mdpPermissionType,

    -- * Destructuring the response
    ModifyDocumentPermissionResponse (..),
    mkModifyDocumentPermissionResponse,

    -- ** Response lenses
    mdprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkModifyDocumentPermission' smart constructor.
data ModifyDocumentPermission = ModifyDocumentPermission'
  { sharedDocumentVersion ::
      Lude.Maybe Lude.Text,
    accountIdsToAdd :: Lude.Maybe [Lude.Text],
    accountIdsToRemove ::
      Lude.Maybe [Lude.Text],
    name :: Lude.Text,
    permissionType :: DocumentPermissionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDocumentPermission' with the minimum fields required to make a request.
--
-- * 'accountIdsToAdd' - The AWS user accounts that should have access to the document. The account IDs can either be a group of account IDs or /All/ .
-- * 'accountIdsToRemove' - The AWS user accounts that should no longer have access to the document. The AWS user account can either be a group of account IDs or /All/ . This action has a higher priority than /AccountIdsToAdd/ . If you specify an account ID to add and the same ID to remove, the system removes access to the document.
-- * 'name' - The name of the document that you want to share.
-- * 'permissionType' - The permission type for the document. The permission type can be /Share/ .
-- * 'sharedDocumentVersion' - (Optional) The version of the document to share. If it's not specified, the system choose the @Default@ version to share.
mkModifyDocumentPermission ::
  -- | 'name'
  Lude.Text ->
  -- | 'permissionType'
  DocumentPermissionType ->
  ModifyDocumentPermission
mkModifyDocumentPermission pName_ pPermissionType_ =
  ModifyDocumentPermission'
    { sharedDocumentVersion = Lude.Nothing,
      accountIdsToAdd = Lude.Nothing,
      accountIdsToRemove = Lude.Nothing,
      name = pName_,
      permissionType = pPermissionType_
    }

-- | (Optional) The version of the document to share. If it's not specified, the system choose the @Default@ version to share.
--
-- /Note:/ Consider using 'sharedDocumentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpSharedDocumentVersion :: Lens.Lens' ModifyDocumentPermission (Lude.Maybe Lude.Text)
mdpSharedDocumentVersion = Lens.lens (sharedDocumentVersion :: ModifyDocumentPermission -> Lude.Maybe Lude.Text) (\s a -> s {sharedDocumentVersion = a} :: ModifyDocumentPermission)
{-# DEPRECATED mdpSharedDocumentVersion "Use generic-lens or generic-optics with 'sharedDocumentVersion' instead." #-}

-- | The AWS user accounts that should have access to the document. The account IDs can either be a group of account IDs or /All/ .
--
-- /Note:/ Consider using 'accountIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpAccountIdsToAdd :: Lens.Lens' ModifyDocumentPermission (Lude.Maybe [Lude.Text])
mdpAccountIdsToAdd = Lens.lens (accountIdsToAdd :: ModifyDocumentPermission -> Lude.Maybe [Lude.Text]) (\s a -> s {accountIdsToAdd = a} :: ModifyDocumentPermission)
{-# DEPRECATED mdpAccountIdsToAdd "Use generic-lens or generic-optics with 'accountIdsToAdd' instead." #-}

-- | The AWS user accounts that should no longer have access to the document. The AWS user account can either be a group of account IDs or /All/ . This action has a higher priority than /AccountIdsToAdd/ . If you specify an account ID to add and the same ID to remove, the system removes access to the document.
--
-- /Note:/ Consider using 'accountIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpAccountIdsToRemove :: Lens.Lens' ModifyDocumentPermission (Lude.Maybe [Lude.Text])
mdpAccountIdsToRemove = Lens.lens (accountIdsToRemove :: ModifyDocumentPermission -> Lude.Maybe [Lude.Text]) (\s a -> s {accountIdsToRemove = a} :: ModifyDocumentPermission)
{-# DEPRECATED mdpAccountIdsToRemove "Use generic-lens or generic-optics with 'accountIdsToRemove' instead." #-}

-- | The name of the document that you want to share.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpName :: Lens.Lens' ModifyDocumentPermission Lude.Text
mdpName = Lens.lens (name :: ModifyDocumentPermission -> Lude.Text) (\s a -> s {name = a} :: ModifyDocumentPermission)
{-# DEPRECATED mdpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The permission type for the document. The permission type can be /Share/ .
--
-- /Note:/ Consider using 'permissionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdpPermissionType :: Lens.Lens' ModifyDocumentPermission DocumentPermissionType
mdpPermissionType = Lens.lens (permissionType :: ModifyDocumentPermission -> DocumentPermissionType) (\s a -> s {permissionType = a} :: ModifyDocumentPermission)
{-# DEPRECATED mdpPermissionType "Use generic-lens or generic-optics with 'permissionType' instead." #-}

instance Lude.AWSRequest ModifyDocumentPermission where
  type Rs ModifyDocumentPermission = ModifyDocumentPermissionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ModifyDocumentPermissionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyDocumentPermission where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ModifyDocumentPermission" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyDocumentPermission where
  toJSON ModifyDocumentPermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SharedDocumentVersion" Lude..=) Lude.<$> sharedDocumentVersion,
            ("AccountIdsToAdd" Lude..=) Lude.<$> accountIdsToAdd,
            ("AccountIdsToRemove" Lude..=) Lude.<$> accountIdsToRemove,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("PermissionType" Lude..= permissionType)
          ]
      )

instance Lude.ToPath ModifyDocumentPermission where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDocumentPermission where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkModifyDocumentPermissionResponse' smart constructor.
newtype ModifyDocumentPermissionResponse = ModifyDocumentPermissionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDocumentPermissionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkModifyDocumentPermissionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyDocumentPermissionResponse
mkModifyDocumentPermissionResponse pResponseStatus_ =
  ModifyDocumentPermissionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdprsResponseStatus :: Lens.Lens' ModifyDocumentPermissionResponse Lude.Int
mdprsResponseStatus = Lens.lens (responseStatus :: ModifyDocumentPermissionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyDocumentPermissionResponse)
{-# DEPRECATED mdprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
