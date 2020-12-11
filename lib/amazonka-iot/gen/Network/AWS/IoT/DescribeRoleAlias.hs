{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeRoleAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a role alias.
module Network.AWS.IoT.DescribeRoleAlias
  ( -- * Creating a request
    DescribeRoleAlias (..),
    mkDescribeRoleAlias,

    -- ** Request lenses
    draRoleAlias,

    -- * Destructuring the response
    DescribeRoleAliasResponse (..),
    mkDescribeRoleAliasResponse,

    -- ** Response lenses
    drarsRoleAliasDescription,
    drarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRoleAlias' smart constructor.
newtype DescribeRoleAlias = DescribeRoleAlias'
  { roleAlias ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRoleAlias' with the minimum fields required to make a request.
--
-- * 'roleAlias' - The role alias to describe.
mkDescribeRoleAlias ::
  -- | 'roleAlias'
  Lude.Text ->
  DescribeRoleAlias
mkDescribeRoleAlias pRoleAlias_ =
  DescribeRoleAlias' {roleAlias = pRoleAlias_}

-- | The role alias to describe.
--
-- /Note:/ Consider using 'roleAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draRoleAlias :: Lens.Lens' DescribeRoleAlias Lude.Text
draRoleAlias = Lens.lens (roleAlias :: DescribeRoleAlias -> Lude.Text) (\s a -> s {roleAlias = a} :: DescribeRoleAlias)
{-# DEPRECATED draRoleAlias "Use generic-lens or generic-optics with 'roleAlias' instead." #-}

instance Lude.AWSRequest DescribeRoleAlias where
  type Rs DescribeRoleAlias = DescribeRoleAliasResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRoleAliasResponse'
            Lude.<$> (x Lude..?> "roleAliasDescription")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRoleAlias where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeRoleAlias where
  toPath DescribeRoleAlias' {..} =
    Lude.mconcat ["/role-aliases/", Lude.toBS roleAlias]

instance Lude.ToQuery DescribeRoleAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRoleAliasResponse' smart constructor.
data DescribeRoleAliasResponse = DescribeRoleAliasResponse'
  { roleAliasDescription ::
      Lude.Maybe RoleAliasDescription,
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

-- | Creates a value of 'DescribeRoleAliasResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'roleAliasDescription' - The role alias description.
mkDescribeRoleAliasResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRoleAliasResponse
mkDescribeRoleAliasResponse pResponseStatus_ =
  DescribeRoleAliasResponse'
    { roleAliasDescription = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The role alias description.
--
-- /Note:/ Consider using 'roleAliasDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drarsRoleAliasDescription :: Lens.Lens' DescribeRoleAliasResponse (Lude.Maybe RoleAliasDescription)
drarsRoleAliasDescription = Lens.lens (roleAliasDescription :: DescribeRoleAliasResponse -> Lude.Maybe RoleAliasDescription) (\s a -> s {roleAliasDescription = a} :: DescribeRoleAliasResponse)
{-# DEPRECATED drarsRoleAliasDescription "Use generic-lens or generic-optics with 'roleAliasDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drarsResponseStatus :: Lens.Lens' DescribeRoleAliasResponse Lude.Int
drarsResponseStatus = Lens.lens (responseStatus :: DescribeRoleAliasResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRoleAliasResponse)
{-# DEPRECATED drarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
