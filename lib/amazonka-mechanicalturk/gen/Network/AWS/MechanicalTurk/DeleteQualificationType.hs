{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.DeleteQualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteQualificationType@ deletes a Qualification type and deletes any HIT types that are associated with the Qualification type.
--
-- This operation does not revoke Qualifications already assigned to Workers because the Qualifications might be needed for active HITs. If there are any pending requests for the Qualification type, Amazon Mechanical Turk rejects those requests. After you delete a Qualification type, you can no longer use it to create HITs or HIT types.
module Network.AWS.MechanicalTurk.DeleteQualificationType
  ( -- * Creating a request
    DeleteQualificationType (..),
    mkDeleteQualificationType,

    -- ** Request lenses
    dqtQualificationTypeId,

    -- * Destructuring the response
    DeleteQualificationTypeResponse (..),
    mkDeleteQualificationTypeResponse,

    -- ** Response lenses
    dqtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteQualificationType' smart constructor.
newtype DeleteQualificationType = DeleteQualificationType'
  { -- | The ID of the QualificationType to dispose.
    qualificationTypeId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQualificationType' with the minimum fields required to make a request.
--
-- * 'qualificationTypeId' - The ID of the QualificationType to dispose.
mkDeleteQualificationType ::
  -- | 'qualificationTypeId'
  Lude.Text ->
  DeleteQualificationType
mkDeleteQualificationType pQualificationTypeId_ =
  DeleteQualificationType'
    { qualificationTypeId =
        pQualificationTypeId_
    }

-- | The ID of the QualificationType to dispose.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqtQualificationTypeId :: Lens.Lens' DeleteQualificationType Lude.Text
dqtQualificationTypeId = Lens.lens (qualificationTypeId :: DeleteQualificationType -> Lude.Text) (\s a -> s {qualificationTypeId = a} :: DeleteQualificationType)
{-# DEPRECATED dqtQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

instance Lude.AWSRequest DeleteQualificationType where
  type Rs DeleteQualificationType = DeleteQualificationTypeResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteQualificationTypeResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteQualificationType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.DeleteQualificationType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteQualificationType where
  toJSON DeleteQualificationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("QualificationTypeId" Lude..= qualificationTypeId)]
      )

instance Lude.ToPath DeleteQualificationType where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteQualificationType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteQualificationTypeResponse' smart constructor.
newtype DeleteQualificationTypeResponse = DeleteQualificationTypeResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQualificationTypeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteQualificationTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteQualificationTypeResponse
mkDeleteQualificationTypeResponse pResponseStatus_ =
  DeleteQualificationTypeResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqtrsResponseStatus :: Lens.Lens' DeleteQualificationTypeResponse Lude.Int
dqtrsResponseStatus = Lens.lens (responseStatus :: DeleteQualificationTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteQualificationTypeResponse)
{-# DEPRECATED dqtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
