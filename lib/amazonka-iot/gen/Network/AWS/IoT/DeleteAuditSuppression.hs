{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteAuditSuppression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Defender audit suppression.
module Network.AWS.IoT.DeleteAuditSuppression
  ( -- * Creating a request
    DeleteAuditSuppression (..),
    mkDeleteAuditSuppression,

    -- ** Request lenses
    dasCheckName,
    dasResourceIdentifier,

    -- * Destructuring the response
    DeleteAuditSuppressionResponse (..),
    mkDeleteAuditSuppressionResponse,

    -- ** Response lenses
    dasrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAuditSuppression' smart constructor.
data DeleteAuditSuppression = DeleteAuditSuppression'
  { checkName :: Lude.Text,
    resourceIdentifier :: ResourceIdentifier
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAuditSuppression' with the minimum fields required to make a request.
--
-- * 'checkName' -
-- * 'resourceIdentifier' -
mkDeleteAuditSuppression ::
  -- | 'checkName'
  Lude.Text ->
  -- | 'resourceIdentifier'
  ResourceIdentifier ->
  DeleteAuditSuppression
mkDeleteAuditSuppression pCheckName_ pResourceIdentifier_ =
  DeleteAuditSuppression'
    { checkName = pCheckName_,
      resourceIdentifier = pResourceIdentifier_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'checkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasCheckName :: Lens.Lens' DeleteAuditSuppression Lude.Text
dasCheckName = Lens.lens (checkName :: DeleteAuditSuppression -> Lude.Text) (\s a -> s {checkName = a} :: DeleteAuditSuppression)
{-# DEPRECATED dasCheckName "Use generic-lens or generic-optics with 'checkName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasResourceIdentifier :: Lens.Lens' DeleteAuditSuppression ResourceIdentifier
dasResourceIdentifier = Lens.lens (resourceIdentifier :: DeleteAuditSuppression -> ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: DeleteAuditSuppression)
{-# DEPRECATED dasResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Lude.AWSRequest DeleteAuditSuppression where
  type Rs DeleteAuditSuppression = DeleteAuditSuppressionResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteAuditSuppressionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAuditSuppression where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DeleteAuditSuppression where
  toJSON DeleteAuditSuppression' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("checkName" Lude..= checkName),
            Lude.Just ("resourceIdentifier" Lude..= resourceIdentifier)
          ]
      )

instance Lude.ToPath DeleteAuditSuppression where
  toPath = Lude.const "/audit/suppressions/delete"

instance Lude.ToQuery DeleteAuditSuppression where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAuditSuppressionResponse' smart constructor.
newtype DeleteAuditSuppressionResponse = DeleteAuditSuppressionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAuditSuppressionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteAuditSuppressionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAuditSuppressionResponse
mkDeleteAuditSuppressionResponse pResponseStatus_ =
  DeleteAuditSuppressionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrsResponseStatus :: Lens.Lens' DeleteAuditSuppressionResponse Lude.Int
dasrsResponseStatus = Lens.lens (responseStatus :: DeleteAuditSuppressionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAuditSuppressionResponse)
{-# DEPRECATED dasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
