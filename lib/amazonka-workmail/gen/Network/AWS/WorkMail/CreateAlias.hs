{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an alias to the set of a given member (user or group) of Amazon WorkMail.
module Network.AWS.WorkMail.CreateAlias
  ( -- * Creating a request
    CreateAlias (..),
    mkCreateAlias,

    -- ** Request lenses
    caAlias,
    caEntityId,
    caOrganizationId,

    -- * Destructuring the response
    CreateAliasResponse (..),
    mkCreateAliasResponse,

    -- ** Response lenses
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | The alias to add to the member set.
    alias :: Lude.Text,
    -- | The member (user or group) to which this alias is added.
    entityId :: Lude.Text,
    -- | The organization under which the member (user or group) exists.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- * 'alias' - The alias to add to the member set.
-- * 'entityId' - The member (user or group) to which this alias is added.
-- * 'organizationId' - The organization under which the member (user or group) exists.
mkCreateAlias ::
  -- | 'alias'
  Lude.Text ->
  -- | 'entityId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  CreateAlias
mkCreateAlias pAlias_ pEntityId_ pOrganizationId_ =
  CreateAlias'
    { alias = pAlias_,
      entityId = pEntityId_,
      organizationId = pOrganizationId_
    }

-- | The alias to add to the member set.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlias :: Lens.Lens' CreateAlias Lude.Text
caAlias = Lens.lens (alias :: CreateAlias -> Lude.Text) (\s a -> s {alias = a} :: CreateAlias)
{-# DEPRECATED caAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The member (user or group) to which this alias is added.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caEntityId :: Lens.Lens' CreateAlias Lude.Text
caEntityId = Lens.lens (entityId :: CreateAlias -> Lude.Text) (\s a -> s {entityId = a} :: CreateAlias)
{-# DEPRECATED caEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The organization under which the member (user or group) exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOrganizationId :: Lens.Lens' CreateAlias Lude.Text
caOrganizationId = Lens.lens (organizationId :: CreateAlias -> Lude.Text) (\s a -> s {organizationId = a} :: CreateAlias)
{-# DEPRECATED caOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest CreateAlias where
  type Rs CreateAlias = CreateAliasResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateAliasResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.CreateAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Alias" Lude..= alias),
            Lude.Just ("EntityId" Lude..= entityId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath CreateAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAliasResponse' smart constructor.
newtype CreateAliasResponse = CreateAliasResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAliasResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAliasResponse
mkCreateAliasResponse pResponseStatus_ =
  CreateAliasResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAliasResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAliasResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
