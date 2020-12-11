{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.DeleteTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A synchronous action that deletes a custom terminology.
module Network.AWS.Translate.DeleteTerminology
  ( -- * Creating a request
    DeleteTerminology (..),
    mkDeleteTerminology,

    -- ** Request lenses
    dtName,

    -- * Destructuring the response
    DeleteTerminologyResponse (..),
    mkDeleteTerminologyResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkDeleteTerminology' smart constructor.
newtype DeleteTerminology = DeleteTerminology' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTerminology' with the minimum fields required to make a request.
--
-- * 'name' - The name of the custom terminology being deleted.
mkDeleteTerminology ::
  -- | 'name'
  Lude.Text ->
  DeleteTerminology
mkDeleteTerminology pName_ = DeleteTerminology' {name = pName_}

-- | The name of the custom terminology being deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtName :: Lens.Lens' DeleteTerminology Lude.Text
dtName = Lens.lens (name :: DeleteTerminology -> Lude.Text) (\s a -> s {name = a} :: DeleteTerminology)
{-# DEPRECATED dtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteTerminology where
  type Rs DeleteTerminology = DeleteTerminologyResponse
  request = Req.postJSON translateService
  response = Res.receiveNull DeleteTerminologyResponse'

instance Lude.ToHeaders DeleteTerminology where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.DeleteTerminology" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTerminology where
  toJSON DeleteTerminology' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteTerminology where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTerminology where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTerminologyResponse' smart constructor.
data DeleteTerminologyResponse = DeleteTerminologyResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTerminologyResponse' with the minimum fields required to make a request.
mkDeleteTerminologyResponse ::
  DeleteTerminologyResponse
mkDeleteTerminologyResponse = DeleteTerminologyResponse'
