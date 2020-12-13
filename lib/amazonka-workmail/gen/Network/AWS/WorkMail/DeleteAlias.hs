{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more specified aliases from a set of aliases for a given user.
module Network.AWS.WorkMail.DeleteAlias
  ( -- * Creating a request
    DeleteAlias (..),
    mkDeleteAlias,

    -- ** Request lenses
    daAlias,
    daEntityId,
    daOrganizationId,

    -- * Destructuring the response
    DeleteAliasResponse (..),
    mkDeleteAliasResponse,

    -- ** Response lenses
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | The aliases to be removed from the user's set of aliases. Duplicate entries in the list are collapsed into single entries (the list is transformed into a set).
    alias :: Lude.Text,
    -- | The identifier for the member (user or group) from which to have the aliases removed.
    entityId :: Lude.Text,
    -- | The identifier for the organization under which the user exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAlias' with the minimum fields required to make a request.
--
-- * 'alias' - The aliases to be removed from the user's set of aliases. Duplicate entries in the list are collapsed into single entries (the list is transformed into a set).
-- * 'entityId' - The identifier for the member (user or group) from which to have the aliases removed.
-- * 'organizationId' - The identifier for the organization under which the user exists.
mkDeleteAlias ::
  -- | 'alias'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  DeleteAlias
mkDeleteAlias pAlias_ pEntityId_ pOrganizationId_ =
  DeleteAlias'
    { alias = pAlias_,
      entityId = pEntityId_,
      organizationId = pOrganizationId_
    }

-- | The aliases to be removed from the user's set of aliases. Duplicate entries in the list are collapsed into single entries (the list is transformed into a set).
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlias :: Lens.Lens' DeleteAlias Lude.Text
daAlias = Lens.lens (alias :: DeleteAlias -> Lude.Text) (\s a -> s {alias = a} :: DeleteAlias)
{-# DEPRECATED daAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The identifier for the member (user or group) from which to have the aliases removed.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daEntityId :: Lens.Lens' DeleteAlias Lude.Text
daEntityId = Lens.lens (entityId :: DeleteAlias -> Lude.Text) (\s a -> s {entityId = a} :: DeleteAlias)
{-# DEPRECATED daEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The identifier for the organization under which the user exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daOrganizationId :: Lens.Lens' DeleteAlias Lude.Text
daOrganizationId = Lens.lens (organizationId :: DeleteAlias -> Lude.Text) (\s a -> s {organizationId = a} :: DeleteAlias)
{-# DEPRECATED daOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest DeleteAlias where
  type Rs DeleteAlias = DeleteAliasResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAliasResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.DeleteAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Alias" Lude..= alias),
            Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath DeleteAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAliasResponse' smart constructor.
newtype DeleteAliasResponse = DeleteAliasResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAliasResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAliasResponse
mkDeleteAliasResponse pResponseStatus_ =
  DeleteAliasResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DeleteAliasResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DeleteAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAliasResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
