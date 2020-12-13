{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.DeleteIntent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the intent, including the @> LATEST@ version. To delete a specific version of the intent, use the 'DeleteIntentVersion' operation.
--
-- You can delete a version of an intent only if it is not referenced. To delete an intent that is referred to in one or more bots (see 'how-it-works' ), you must remove those references first.
-- This operation requires permission for the @lex:DeleteIntent@ action.
module Network.AWS.LexModels.DeleteIntent
  ( -- * Creating a request
    DeleteIntent (..),
    mkDeleteIntent,

    -- ** Request lenses
    diName,

    -- * Destructuring the response
    DeleteIntentResponse (..),
    mkDeleteIntentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteIntent' smart constructor.
newtype DeleteIntent = DeleteIntent'
  { -- | The name of the intent. The name is case sensitive.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIntent' with the minimum fields required to make a request.
--
-- * 'name' - The name of the intent. The name is case sensitive.
mkDeleteIntent ::
  -- | 'name'
  Lude.Text ->
  DeleteIntent
mkDeleteIntent pName_ = DeleteIntent' {name = pName_}

-- | The name of the intent. The name is case sensitive.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DeleteIntent Lude.Text
diName = Lens.lens (name :: DeleteIntent -> Lude.Text) (\s a -> s {name = a} :: DeleteIntent)
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteIntent where
  type Rs DeleteIntent = DeleteIntentResponse
  request = Req.delete lexModelsService
  response = Res.receiveNull DeleteIntentResponse'

instance Lude.ToHeaders DeleteIntent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteIntent where
  toPath DeleteIntent' {..} =
    Lude.mconcat ["/intents/", Lude.toBS name]

instance Lude.ToQuery DeleteIntent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteIntentResponse' smart constructor.
data DeleteIntentResponse = DeleteIntentResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIntentResponse' with the minimum fields required to make a request.
mkDeleteIntentResponse ::
  DeleteIntentResponse
mkDeleteIntentResponse = DeleteIntentResponse'
