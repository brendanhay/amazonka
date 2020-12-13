{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteIntentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of an intent. To delete all versions of a intent, use the 'DeleteIntent' operation.
--
-- This operation requires permissions for the @lex:DeleteIntentVersion@ action.
module Network.AWS.LexModels.DeleteIntentVersion
  ( -- * Creating a request
    DeleteIntentVersion (..),
    mkDeleteIntentVersion,

    -- ** Request lenses
    divName,
    divVersion,

    -- * Destructuring the response
    DeleteIntentVersionResponse (..),
    mkDeleteIntentVersionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteIntentVersion' smart constructor.
data DeleteIntentVersion = DeleteIntentVersion'
  { -- | The name of the intent.
    name :: Lude.Text,
    -- | The version of the intent to delete. You cannot delete the @> LATEST@ version of the intent. To delete the @> LATEST@ version, use the 'DeleteIntent' operation.
    version :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIntentVersion' with the minimum fields required to make a request.
--
-- * 'name' - The name of the intent.
-- * 'version' - The version of the intent to delete. You cannot delete the @> LATEST@ version of the intent. To delete the @> LATEST@ version, use the 'DeleteIntent' operation.
mkDeleteIntentVersion ::
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  DeleteIntentVersion
mkDeleteIntentVersion pName_ pVersion_ =
  DeleteIntentVersion' {name = pName_, version = pVersion_}

-- | The name of the intent.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divName :: Lens.Lens' DeleteIntentVersion Lude.Text
divName = Lens.lens (name :: DeleteIntentVersion -> Lude.Text) (\s a -> s {name = a} :: DeleteIntentVersion)
{-# DEPRECATED divName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the intent to delete. You cannot delete the @> LATEST@ version of the intent. To delete the @> LATEST@ version, use the 'DeleteIntent' operation.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
divVersion :: Lens.Lens' DeleteIntentVersion Lude.Text
divVersion = Lens.lens (version :: DeleteIntentVersion -> Lude.Text) (\s a -> s {version = a} :: DeleteIntentVersion)
{-# DEPRECATED divVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest DeleteIntentVersion where
  type Rs DeleteIntentVersion = DeleteIntentVersionResponse
  request = Req.delete lexModelsService
  response = Res.receiveNull DeleteIntentVersionResponse'

instance Lude.ToHeaders DeleteIntentVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteIntentVersion where
  toPath DeleteIntentVersion' {..} =
    Lude.mconcat
      ["/intents/", Lude.toBS name, "/versions/", Lude.toBS version]

instance Lude.ToQuery DeleteIntentVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteIntentVersionResponse' smart constructor.
data DeleteIntentVersionResponse = DeleteIntentVersionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIntentVersionResponse' with the minimum fields required to make a request.
mkDeleteIntentVersionResponse ::
  DeleteIntentVersionResponse
mkDeleteIntentVersionResponse = DeleteIntentVersionResponse'
