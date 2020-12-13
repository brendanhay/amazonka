{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.DeleteAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alias. You cannot perform this operation on an alias in a different AWS account.
--
-- Because an alias is not a property of a CMK, you can delete and change the aliases of a CMK without affecting the CMK. Also, aliases do not appear in the response from the 'DescribeKey' operation. To get the aliases of all CMKs, use the 'ListAliases' operation.
-- Each CMK can have multiple aliases. To change the alias of a CMK, use 'DeleteAlias' to delete the current alias and 'CreateAlias' to create a new alias. To associate an existing alias with a different customer master key (CMK), call 'UpdateAlias' .
module Network.AWS.KMS.DeleteAlias
  ( -- * Creating a request
    DeleteAlias (..),
    mkDeleteAlias,

    -- ** Request lenses
    daAliasName,

    -- * Destructuring the response
    DeleteAliasResponse (..),
    mkDeleteAliasResponse,
  )
where

import Network.AWS.KMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAlias' smart constructor.
newtype DeleteAlias = DeleteAlias'
  { -- | The alias to be deleted. The alias name must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ .
    aliasName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAlias' with the minimum fields required to make a request.
--
-- * 'aliasName' - The alias to be deleted. The alias name must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ .
mkDeleteAlias ::
  -- | 'aliasName'
  Lude.Text ->
  DeleteAlias
mkDeleteAlias pAliasName_ = DeleteAlias' {aliasName = pAliasName_}

-- | The alias to be deleted. The alias name must begin with @alias/@ followed by the alias name, such as @alias/ExampleAlias@ .
--
-- /Note:/ Consider using 'aliasName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAliasName :: Lens.Lens' DeleteAlias Lude.Text
daAliasName = Lens.lens (aliasName :: DeleteAlias -> Lude.Text) (\s a -> s {aliasName = a} :: DeleteAlias)
{-# DEPRECATED daAliasName "Use generic-lens or generic-optics with 'aliasName' instead." #-}

instance Lude.AWSRequest DeleteAlias where
  type Rs DeleteAlias = DeleteAliasResponse
  request = Req.postJSON kmsService
  response = Res.receiveNull DeleteAliasResponse'

instance Lude.ToHeaders DeleteAlias where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("TrentService.DeleteAlias" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AliasName" Lude..= aliasName)])

instance Lude.ToPath DeleteAlias where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAlias where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAliasResponse' with the minimum fields required to make a request.
mkDeleteAliasResponse ::
  DeleteAliasResponse
mkDeleteAliasResponse = DeleteAliasResponse'
