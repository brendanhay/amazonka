{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DeleteConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified constraint.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DeleteConstraint
  ( -- * Creating a request
    DeleteConstraint (..),
    mkDeleteConstraint,

    -- ** Request lenses
    dcAcceptLanguage,
    dcId,

    -- * Destructuring the response
    DeleteConstraintResponse (..),
    mkDeleteConstraintResponse,

    -- ** Response lenses
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkDeleteConstraint' smart constructor.
data DeleteConstraint = DeleteConstraint'
  { acceptLanguage ::
      Lude.Maybe Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConstraint' with the minimum fields required to make a request.
--
-- * 'acceptLanguage' - The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
-- * 'id' - The identifier of the constraint.
mkDeleteConstraint ::
  -- | 'id'
  Lude.Text ->
  DeleteConstraint
mkDeleteConstraint pId_ =
  DeleteConstraint' {acceptLanguage = Lude.Nothing, id = pId_}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAcceptLanguage :: Lens.Lens' DeleteConstraint (Lude.Maybe Lude.Text)
dcAcceptLanguage = Lens.lens (acceptLanguage :: DeleteConstraint -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: DeleteConstraint)
{-# DEPRECATED dcAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The identifier of the constraint.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcId :: Lens.Lens' DeleteConstraint Lude.Text
dcId = Lens.lens (id :: DeleteConstraint -> Lude.Text) (\s a -> s {id = a} :: DeleteConstraint)
{-# DEPRECATED dcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest DeleteConstraint where
  type Rs DeleteConstraint = DeleteConstraintResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteConstraintResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteConstraint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.DeleteConstraint" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteConstraint where
  toJSON DeleteConstraint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("Id" Lude..= id)
          ]
      )

instance Lude.ToPath DeleteConstraint where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteConstraint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteConstraintResponse' smart constructor.
newtype DeleteConstraintResponse = DeleteConstraintResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteConstraintResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteConstraintResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteConstraintResponse
mkDeleteConstraintResponse pResponseStatus_ =
  DeleteConstraintResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DeleteConstraintResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DeleteConstraintResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteConstraintResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
