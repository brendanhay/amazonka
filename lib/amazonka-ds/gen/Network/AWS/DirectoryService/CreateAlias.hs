{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a directory and assigns the alias to the directory. The alias is used to construct the access URL for the directory, such as @http://<alias>.awsapps.com@ .
--
-- /Important:/ After an alias has been created, it cannot be deleted or reused, so this operation should only be used when absolutely necessary.
module Network.AWS.DirectoryService.CreateAlias
  ( -- * Creating a request
    CreateAlias (..),
    mkCreateAlias,

    -- ** Request lenses
    caDirectoryId,
    caAlias,

    -- * Destructuring the response
    CreateAliasResponse (..),
    mkCreateAliasResponse,

    -- ** Response lenses
    carsDirectoryId,
    carsAlias,
    carsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'CreateAlias' operation.
--
-- /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { -- | The identifier of the directory for which to create the alias.
    directoryId :: Lude.Text,
    -- | The requested alias.
    --
    -- The alias must be unique amongst all aliases in AWS. This operation throws an @EntityAlreadyExistsException@ error if the alias already exists.
    alias :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory for which to create the alias.
-- * 'alias' - The requested alias.
--
-- The alias must be unique amongst all aliases in AWS. This operation throws an @EntityAlreadyExistsException@ error if the alias already exists.
mkCreateAlias ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'alias'
  Lude.Text ->
  CreateAlias
mkCreateAlias pDirectoryId_ pAlias_ =
  CreateAlias' {directoryId = pDirectoryId_, alias = pAlias_}

-- | The identifier of the directory for which to create the alias.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDirectoryId :: Lens.Lens' CreateAlias Lude.Text
caDirectoryId = Lens.lens (directoryId :: CreateAlias -> Lude.Text) (\s a -> s {directoryId = a} :: CreateAlias)
{-# DEPRECATED caDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The requested alias.
--
-- The alias must be unique amongst all aliases in AWS. This operation throws an @EntityAlreadyExistsException@ error if the alias already exists.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAlias :: Lens.Lens' CreateAlias Lude.Text
caAlias = Lens.lens (alias :: CreateAlias -> Lude.Text) (\s a -> s {alias = a} :: CreateAlias)
{-# DEPRECATED caAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

instance Lude.AWSRequest CreateAlias where
  type Rs CreateAlias = CreateAliasResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAliasResponse'
            Lude.<$> (x Lude..?> "DirectoryId")
            Lude.<*> (x Lude..?> "Alias")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.CreateAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAlias where
  toJSON CreateAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("Alias" Lude..= alias)
          ]
      )

instance Lude.ToPath CreateAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAlias where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'CreateAlias' operation.
--
-- /See:/ 'mkCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { -- | The identifier of the directory.
    directoryId :: Lude.Maybe Lude.Text,
    -- | The alias for the directory.
    alias :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAliasResponse' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory.
-- * 'alias' - The alias for the directory.
-- * 'responseStatus' - The response status code.
mkCreateAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAliasResponse
mkCreateAliasResponse pResponseStatus_ =
  CreateAliasResponse'
    { directoryId = Lude.Nothing,
      alias = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsDirectoryId :: Lens.Lens' CreateAliasResponse (Lude.Maybe Lude.Text)
carsDirectoryId = Lens.lens (directoryId :: CreateAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: CreateAliasResponse)
{-# DEPRECATED carsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The alias for the directory.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAlias :: Lens.Lens' CreateAliasResponse (Lude.Maybe Lude.Text)
carsAlias = Lens.lens (alias :: CreateAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {alias = a} :: CreateAliasResponse)
{-# DEPRECATED carsAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAliasResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAliasResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
