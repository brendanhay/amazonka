{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateRoleAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a role alias.
module Network.AWS.IoT.CreateRoleAlias
  ( -- * Creating a request
    CreateRoleAlias (..),
    mkCreateRoleAlias,

    -- ** Request lenses
    craCredentialDurationSeconds,
    craTags,
    craRoleAlias,
    craRoleARN,

    -- * Destructuring the response
    CreateRoleAliasResponse (..),
    mkCreateRoleAliasResponse,

    -- ** Response lenses
    crarsRoleAliasARN,
    crarsRoleAlias,
    crarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateRoleAlias' smart constructor.
data CreateRoleAlias = CreateRoleAlias'
  { credentialDurationSeconds ::
      Lude.Maybe Lude.Natural,
    tags :: Lude.Maybe [Tag],
    roleAlias :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRoleAlias' with the minimum fields required to make a request.
--
-- * 'credentialDurationSeconds' - How long (in seconds) the credentials will be valid.
-- * 'roleARN' - The role ARN.
-- * 'roleAlias' - The role alias that points to a role ARN. This allows you to change the role without having to update the device.
-- * 'tags' - Metadata which can be used to manage the role alias.
mkCreateRoleAlias ::
  -- | 'roleAlias'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CreateRoleAlias
mkCreateRoleAlias pRoleAlias_ pRoleARN_ =
  CreateRoleAlias'
    { credentialDurationSeconds = Lude.Nothing,
      tags = Lude.Nothing,
      roleAlias = pRoleAlias_,
      roleARN = pRoleARN_
    }

-- | How long (in seconds) the credentials will be valid.
--
-- /Note:/ Consider using 'credentialDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craCredentialDurationSeconds :: Lens.Lens' CreateRoleAlias (Lude.Maybe Lude.Natural)
craCredentialDurationSeconds = Lens.lens (credentialDurationSeconds :: CreateRoleAlias -> Lude.Maybe Lude.Natural) (\s a -> s {credentialDurationSeconds = a} :: CreateRoleAlias)
{-# DEPRECATED craCredentialDurationSeconds "Use generic-lens or generic-optics with 'credentialDurationSeconds' instead." #-}

-- | Metadata which can be used to manage the role alias.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craTags :: Lens.Lens' CreateRoleAlias (Lude.Maybe [Tag])
craTags = Lens.lens (tags :: CreateRoleAlias -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateRoleAlias)
{-# DEPRECATED craTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The role alias that points to a role ARN. This allows you to change the role without having to update the device.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craRoleAlias :: Lens.Lens' CreateRoleAlias Lude.Text
craRoleAlias = Lens.lens (roleAlias :: CreateRoleAlias -> Lude.Text) (\s a -> s {roleAlias = a} :: CreateRoleAlias)
{-# DEPRECATED craRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

-- | The role ARN.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craRoleARN :: Lens.Lens' CreateRoleAlias Lude.Text
craRoleARN = Lens.lens (roleARN :: CreateRoleAlias -> Lude.Text) (\s a -> s {roleARN = a} :: CreateRoleAlias)
{-# DEPRECATED craRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateRoleAlias where
  type Rs CreateRoleAlias = CreateRoleAliasResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRoleAliasResponse'
            Lude.<$> (x Lude..?> "roleAliasArn")
            Lude.<*> (x Lude..?> "roleAlias")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRoleAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateRoleAlias where
  toJSON CreateRoleAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("credentialDurationSeconds" Lude..=)
              Lude.<$> credentialDurationSeconds,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateRoleAlias where
  toPath CreateRoleAlias' {..} =
    Lude.mconcat ["/role-aliases/", Lude.toBS roleAlias]

instance Lude.ToQuery CreateRoleAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateRoleAliasResponse' smart constructor.
data CreateRoleAliasResponse = CreateRoleAliasResponse'
  { roleAliasARN ::
      Lude.Maybe Lude.Text,
    roleAlias :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRoleAliasResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'roleAlias' - The role alias.
-- * 'roleAliasARN' - The role alias ARN.
mkCreateRoleAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRoleAliasResponse
mkCreateRoleAliasResponse pResponseStatus_ =
  CreateRoleAliasResponse'
    { roleAliasARN = Lude.Nothing,
      roleAlias = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The role alias ARN.
--
-- /Note:/ Consider using 'roleAliasARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crarsRoleAliasARN :: Lens.Lens' CreateRoleAliasResponse (Lude.Maybe Lude.Text)
crarsRoleAliasARN = Lens.lens (roleAliasARN :: CreateRoleAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleAliasARN = a} :: CreateRoleAliasResponse)
{-# DEPRECATED crarsRoleAliasARN "Use generic-lens or generic-optics with 'roleAliasARN' instead." #-}

-- | The role alias.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crarsRoleAlias :: Lens.Lens' CreateRoleAliasResponse (Lude.Maybe Lude.Text)
crarsRoleAlias = Lens.lens (roleAlias :: CreateRoleAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleAlias = a} :: CreateRoleAliasResponse)
{-# DEPRECATED crarsRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crarsResponseStatus :: Lens.Lens' CreateRoleAliasResponse Lude.Int
crarsResponseStatus = Lens.lens (responseStatus :: CreateRoleAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRoleAliasResponse)
{-# DEPRECATED crarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
