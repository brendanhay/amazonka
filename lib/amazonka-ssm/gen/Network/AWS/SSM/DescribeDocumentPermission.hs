{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeDocumentPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the permissions for a Systems Manager document. If you created the document, you are the owner. If a document is shared, it can either be shared privately (by specifying a user's AWS account ID) or publicly (/All/ ).
module Network.AWS.SSM.DescribeDocumentPermission
  ( -- * Creating a request
    DescribeDocumentPermission (..),
    mkDescribeDocumentPermission,

    -- ** Request lenses
    ddpPermissionType,
    ddpName,

    -- * Destructuring the response
    DescribeDocumentPermissionResponse (..),
    mkDescribeDocumentPermissionResponse,

    -- ** Response lenses
    ddprsAccountIds,
    ddprsAccountSharingInfoList,
    ddprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribeDocumentPermission' smart constructor.
data DescribeDocumentPermission = DescribeDocumentPermission'
  { -- | The permission type for the document. The permission type can be /Share/ .
    permissionType :: DocumentPermissionType,
    -- | The name of the document for which you are the owner.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDocumentPermission' with the minimum fields required to make a request.
--
-- * 'permissionType' - The permission type for the document. The permission type can be /Share/ .
-- * 'name' - The name of the document for which you are the owner.
mkDescribeDocumentPermission ::
  -- | 'permissionType'
  DocumentPermissionType ->
  -- | 'name'
  Lude.Text ->
  DescribeDocumentPermission
mkDescribeDocumentPermission pPermissionType_ pName_ =
  DescribeDocumentPermission'
    { permissionType = pPermissionType_,
      name = pName_
    }

-- | The permission type for the document. The permission type can be /Share/ .
--
-- /Note:/ Consider using 'permissionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpPermissionType :: Lens.Lens' DescribeDocumentPermission DocumentPermissionType
ddpPermissionType = Lens.lens (permissionType :: DescribeDocumentPermission -> DocumentPermissionType) (\s a -> s {permissionType = a} :: DescribeDocumentPermission)
{-# DEPRECATED ddpPermissionType "Use generic-lens or generic-optics with 'permissionType' instead." #-}

-- | The name of the document for which you are the owner.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpName :: Lens.Lens' DescribeDocumentPermission Lude.Text
ddpName = Lens.lens (name :: DescribeDocumentPermission -> Lude.Text) (\s a -> s {name = a} :: DescribeDocumentPermission)
{-# DEPRECATED ddpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribeDocumentPermission where
  type
    Rs DescribeDocumentPermission =
      DescribeDocumentPermissionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDocumentPermissionResponse'
            Lude.<$> (x Lude..?> "AccountIds" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "AccountSharingInfoList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDocumentPermission where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribeDocumentPermission" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDocumentPermission where
  toJSON DescribeDocumentPermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PermissionType" Lude..= permissionType),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath DescribeDocumentPermission where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDocumentPermission where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDocumentPermissionResponse' smart constructor.
data DescribeDocumentPermissionResponse = DescribeDocumentPermissionResponse'
  { -- | The account IDs that have permission to use this document. The ID can be either an AWS account or /All/ .
    accountIds :: Lude.Maybe [Lude.Text],
    -- | A list of AWS accounts where the current document is shared and the version shared with each account.
    accountSharingInfoList :: Lude.Maybe [AccountSharingInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDocumentPermissionResponse' with the minimum fields required to make a request.
--
-- * 'accountIds' - The account IDs that have permission to use this document. The ID can be either an AWS account or /All/ .
-- * 'accountSharingInfoList' - A list of AWS accounts where the current document is shared and the version shared with each account.
-- * 'responseStatus' - The response status code.
mkDescribeDocumentPermissionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDocumentPermissionResponse
mkDescribeDocumentPermissionResponse pResponseStatus_ =
  DescribeDocumentPermissionResponse'
    { accountIds = Lude.Nothing,
      accountSharingInfoList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The account IDs that have permission to use this document. The ID can be either an AWS account or /All/ .
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsAccountIds :: Lens.Lens' DescribeDocumentPermissionResponse (Lude.Maybe [Lude.Text])
ddprsAccountIds = Lens.lens (accountIds :: DescribeDocumentPermissionResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {accountIds = a} :: DescribeDocumentPermissionResponse)
{-# DEPRECATED ddprsAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | A list of AWS accounts where the current document is shared and the version shared with each account.
--
-- /Note:/ Consider using 'accountSharingInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsAccountSharingInfoList :: Lens.Lens' DescribeDocumentPermissionResponse (Lude.Maybe [AccountSharingInfo])
ddprsAccountSharingInfoList = Lens.lens (accountSharingInfoList :: DescribeDocumentPermissionResponse -> Lude.Maybe [AccountSharingInfo]) (\s a -> s {accountSharingInfoList = a} :: DescribeDocumentPermissionResponse)
{-# DEPRECATED ddprsAccountSharingInfoList "Use generic-lens or generic-optics with 'accountSharingInfoList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsResponseStatus :: Lens.Lens' DescribeDocumentPermissionResponse Lude.Int
ddprsResponseStatus = Lens.lens (responseStatus :: DescribeDocumentPermissionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDocumentPermissionResponse)
{-# DEPRECATED ddprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
