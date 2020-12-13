{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.DeleteIdentities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes identities from an identity pool. You can specify a list of 1-60 identities that you want to delete.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DeleteIdentities
  ( -- * Creating a request
    DeleteIdentities (..),
    mkDeleteIdentities,

    -- ** Request lenses
    diIdentityIdsToDelete,

    -- * Destructuring the response
    DeleteIdentitiesResponse (..),
    mkDeleteIdentitiesResponse,

    -- ** Response lenses
    dirsUnprocessedIdentityIds,
    dirsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the @DeleteIdentities@ action.
--
-- /See:/ 'mkDeleteIdentities' smart constructor.
newtype DeleteIdentities = DeleteIdentities'
  { -- | A list of 1-60 identities that you want to delete.
    identityIdsToDelete :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIdentities' with the minimum fields required to make a request.
--
-- * 'identityIdsToDelete' - A list of 1-60 identities that you want to delete.
mkDeleteIdentities ::
  -- | 'identityIdsToDelete'
  Lude.NonEmpty Lude.Text ->
  DeleteIdentities
mkDeleteIdentities pIdentityIdsToDelete_ =
  DeleteIdentities' {identityIdsToDelete = pIdentityIdsToDelete_}

-- | A list of 1-60 identities that you want to delete.
--
-- /Note:/ Consider using 'identityIdsToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diIdentityIdsToDelete :: Lens.Lens' DeleteIdentities (Lude.NonEmpty Lude.Text)
diIdentityIdsToDelete = Lens.lens (identityIdsToDelete :: DeleteIdentities -> Lude.NonEmpty Lude.Text) (\s a -> s {identityIdsToDelete = a} :: DeleteIdentities)
{-# DEPRECATED diIdentityIdsToDelete "Use generic-lens or generic-optics with 'identityIdsToDelete' instead." #-}

instance Lude.AWSRequest DeleteIdentities where
  type Rs DeleteIdentities = DeleteIdentitiesResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteIdentitiesResponse'
            Lude.<$> (x Lude..?> "UnprocessedIdentityIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteIdentities where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityService.DeleteIdentities" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteIdentities where
  toJSON DeleteIdentities' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("IdentityIdsToDelete" Lude..= identityIdsToDelete)]
      )

instance Lude.ToPath DeleteIdentities where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteIdentities where
  toQuery = Lude.const Lude.mempty

-- | Returned in response to a successful @DeleteIdentities@ operation.
--
-- /See:/ 'mkDeleteIdentitiesResponse' smart constructor.
data DeleteIdentitiesResponse = DeleteIdentitiesResponse'
  { -- | An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.
    unprocessedIdentityIds :: Lude.Maybe [UnprocessedIdentityId],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIdentitiesResponse' with the minimum fields required to make a request.
--
-- * 'unprocessedIdentityIds' - An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.
-- * 'responseStatus' - The response status code.
mkDeleteIdentitiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteIdentitiesResponse
mkDeleteIdentitiesResponse pResponseStatus_ =
  DeleteIdentitiesResponse'
    { unprocessedIdentityIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of UnprocessedIdentityId objects, each of which contains an ErrorCode and IdentityId.
--
-- /Note:/ Consider using 'unprocessedIdentityIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsUnprocessedIdentityIds :: Lens.Lens' DeleteIdentitiesResponse (Lude.Maybe [UnprocessedIdentityId])
dirsUnprocessedIdentityIds = Lens.lens (unprocessedIdentityIds :: DeleteIdentitiesResponse -> Lude.Maybe [UnprocessedIdentityId]) (\s a -> s {unprocessedIdentityIds = a} :: DeleteIdentitiesResponse)
{-# DEPRECATED dirsUnprocessedIdentityIds "Use generic-lens or generic-optics with 'unprocessedIdentityIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirsResponseStatus :: Lens.Lens' DeleteIdentitiesResponse Lude.Int
dirsResponseStatus = Lens.lens (responseStatus :: DeleteIdentitiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteIdentitiesResponse)
{-# DEPRECATED dirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
