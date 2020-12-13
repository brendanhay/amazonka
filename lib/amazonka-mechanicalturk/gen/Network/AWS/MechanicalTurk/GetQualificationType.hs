{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetQualificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetQualificationType@ operation retrieves information about a Qualification type using its ID.
module Network.AWS.MechanicalTurk.GetQualificationType
  ( -- * Creating a request
    GetQualificationType (..),
    mkGetQualificationType,

    -- ** Request lenses
    gqtQualificationTypeId,

    -- * Destructuring the response
    GetQualificationTypeResponse (..),
    mkGetQualificationTypeResponse,

    -- ** Response lenses
    gqtrsQualificationType,
    gqtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetQualificationType' smart constructor.
newtype GetQualificationType = GetQualificationType'
  { -- | The ID of the QualificationType.
    qualificationTypeId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQualificationType' with the minimum fields required to make a request.
--
-- * 'qualificationTypeId' - The ID of the QualificationType.
mkGetQualificationType ::
  -- | 'qualificationTypeId'
  Lude.Text ->
  GetQualificationType
mkGetQualificationType pQualificationTypeId_ =
  GetQualificationType'
    { qualificationTypeId =
        pQualificationTypeId_
    }

-- | The ID of the QualificationType.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqtQualificationTypeId :: Lens.Lens' GetQualificationType Lude.Text
gqtQualificationTypeId = Lens.lens (qualificationTypeId :: GetQualificationType -> Lude.Text) (\s a -> s {qualificationTypeId = a} :: GetQualificationType)
{-# DEPRECATED gqtQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

instance Lude.AWSRequest GetQualificationType where
  type Rs GetQualificationType = GetQualificationTypeResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetQualificationTypeResponse'
            Lude.<$> (x Lude..?> "QualificationType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetQualificationType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.GetQualificationType" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetQualificationType where
  toJSON GetQualificationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("QualificationTypeId" Lude..= qualificationTypeId)]
      )

instance Lude.ToPath GetQualificationType where
  toPath = Lude.const "/"

instance Lude.ToQuery GetQualificationType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetQualificationTypeResponse' smart constructor.
data GetQualificationTypeResponse = GetQualificationTypeResponse'
  { -- | The returned Qualification Type
    qualificationType :: Lude.Maybe QualificationType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQualificationTypeResponse' with the minimum fields required to make a request.
--
-- * 'qualificationType' - The returned Qualification Type
-- * 'responseStatus' - The response status code.
mkGetQualificationTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetQualificationTypeResponse
mkGetQualificationTypeResponse pResponseStatus_ =
  GetQualificationTypeResponse'
    { qualificationType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The returned Qualification Type
--
-- /Note:/ Consider using 'qualificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqtrsQualificationType :: Lens.Lens' GetQualificationTypeResponse (Lude.Maybe QualificationType)
gqtrsQualificationType = Lens.lens (qualificationType :: GetQualificationTypeResponse -> Lude.Maybe QualificationType) (\s a -> s {qualificationType = a} :: GetQualificationTypeResponse)
{-# DEPRECATED gqtrsQualificationType "Use generic-lens or generic-optics with 'qualificationType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqtrsResponseStatus :: Lens.Lens' GetQualificationTypeResponse Lude.Int
gqtrsResponseStatus = Lens.lens (responseStatus :: GetQualificationTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetQualificationTypeResponse)
{-# DEPRECATED gqtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
