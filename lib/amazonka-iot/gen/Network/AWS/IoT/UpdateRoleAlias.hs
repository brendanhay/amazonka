{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateRoleAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a role alias.
module Network.AWS.IoT.UpdateRoleAlias
  ( -- * Creating a request
    UpdateRoleAlias (..),
    mkUpdateRoleAlias,

    -- ** Request lenses
    uraRoleAlias,
    uraCredentialDurationSeconds,
    uraRoleARN,

    -- * Destructuring the response
    UpdateRoleAliasResponse (..),
    mkUpdateRoleAliasResponse,

    -- ** Response lenses
    urarsRoleAliasARN,
    urarsRoleAlias,
    urarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateRoleAlias' smart constructor.
data UpdateRoleAlias = UpdateRoleAlias'
  { -- | The role alias to update.
    roleAlias :: Lude.Text,
    -- | The number of seconds the credential will be valid.
    credentialDurationSeconds :: Lude.Maybe Lude.Natural,
    -- | The role ARN.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoleAlias' with the minimum fields required to make a request.
--
-- * 'roleAlias' - The role alias to update.
-- * 'credentialDurationSeconds' - The number of seconds the credential will be valid.
-- * 'roleARN' - The role ARN.
mkUpdateRoleAlias ::
  -- | 'roleAlias'
  Lude.Text ->
  UpdateRoleAlias
mkUpdateRoleAlias pRoleAlias_ =
  UpdateRoleAlias'
    { roleAlias = pRoleAlias_,
      credentialDurationSeconds = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The role alias to update.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraRoleAlias :: Lens.Lens' UpdateRoleAlias Lude.Text
uraRoleAlias = Lens.lens (roleAlias :: UpdateRoleAlias -> Lude.Text) (\s a -> s {roleAlias = a} :: UpdateRoleAlias)
{-# DEPRECATED uraRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

-- | The number of seconds the credential will be valid.
--
-- /Note:/ Consider using 'credentialDurationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraCredentialDurationSeconds :: Lens.Lens' UpdateRoleAlias (Lude.Maybe Lude.Natural)
uraCredentialDurationSeconds = Lens.lens (credentialDurationSeconds :: UpdateRoleAlias -> Lude.Maybe Lude.Natural) (\s a -> s {credentialDurationSeconds = a} :: UpdateRoleAlias)
{-# DEPRECATED uraCredentialDurationSeconds "Use generic-lens or generic-optics with 'credentialDurationSeconds' instead." #-}

-- | The role ARN.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uraRoleARN :: Lens.Lens' UpdateRoleAlias (Lude.Maybe Lude.Text)
uraRoleARN = Lens.lens (roleARN :: UpdateRoleAlias -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateRoleAlias)
{-# DEPRECATED uraRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest UpdateRoleAlias where
  type Rs UpdateRoleAlias = UpdateRoleAliasResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateRoleAliasResponse'
            Lude.<$> (x Lude..?> "roleAliasArn")
            Lude.<*> (x Lude..?> "roleAlias")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateRoleAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateRoleAlias where
  toJSON UpdateRoleAlias' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("credentialDurationSeconds" Lude..=)
              Lude.<$> credentialDurationSeconds,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateRoleAlias where
  toPath UpdateRoleAlias' {..} =
    Lude.mconcat ["/role-aliases/", Lude.toBS roleAlias]

instance Lude.ToQuery UpdateRoleAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRoleAliasResponse' smart constructor.
data UpdateRoleAliasResponse = UpdateRoleAliasResponse'
  { -- | The role alias ARN.
    roleAliasARN :: Lude.Maybe Lude.Text,
    -- | The role alias.
    roleAlias :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRoleAliasResponse' with the minimum fields required to make a request.
--
-- * 'roleAliasARN' - The role alias ARN.
-- * 'roleAlias' - The role alias.
-- * 'responseStatus' - The response status code.
mkUpdateRoleAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateRoleAliasResponse
mkUpdateRoleAliasResponse pResponseStatus_ =
  UpdateRoleAliasResponse'
    { roleAliasARN = Lude.Nothing,
      roleAlias = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The role alias ARN.
--
-- /Note:/ Consider using 'roleAliasARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urarsRoleAliasARN :: Lens.Lens' UpdateRoleAliasResponse (Lude.Maybe Lude.Text)
urarsRoleAliasARN = Lens.lens (roleAliasARN :: UpdateRoleAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleAliasARN = a} :: UpdateRoleAliasResponse)
{-# DEPRECATED urarsRoleAliasARN "Use generic-lens or generic-optics with 'roleAliasARN' instead." #-}

-- | The role alias.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urarsRoleAlias :: Lens.Lens' UpdateRoleAliasResponse (Lude.Maybe Lude.Text)
urarsRoleAlias = Lens.lens (roleAlias :: UpdateRoleAliasResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleAlias = a} :: UpdateRoleAliasResponse)
{-# DEPRECATED urarsRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urarsResponseStatus :: Lens.Lens' UpdateRoleAliasResponse Lude.Int
urarsResponseStatus = Lens.lens (responseStatus :: UpdateRoleAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateRoleAliasResponse)
{-# DEPRECATED urarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
