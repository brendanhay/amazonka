{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetQualificationScore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetQualificationScore@ operation returns the value of a Worker's Qualification for a given Qualification type.
--
-- To get a Worker's Qualification, you must know the Worker's ID. The Worker's ID is included in the assignment data returned by the @ListAssignmentsForHIT@ operation.
-- Only the owner of a Qualification type can query the value of a Worker's Qualification of that type.
module Network.AWS.MechanicalTurk.GetQualificationScore
  ( -- * Creating a request
    GetQualificationScore (..),
    mkGetQualificationScore,

    -- ** Request lenses
    gqsQualificationTypeId,
    gqsWorkerId,

    -- * Destructuring the response
    GetQualificationScoreResponse (..),
    mkGetQualificationScoreResponse,

    -- ** Response lenses
    gqsrsQualification,
    gqsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetQualificationScore' smart constructor.
data GetQualificationScore = GetQualificationScore'
  { qualificationTypeId ::
      Lude.Text,
    workerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetQualificationScore' with the minimum fields required to make a request.
--
-- * 'qualificationTypeId' - The ID of the QualificationType.
-- * 'workerId' - The ID of the Worker whose Qualification is being updated.
mkGetQualificationScore ::
  -- | 'qualificationTypeId'
  Lude.Text ->
  -- | 'workerId'
  Lude.Text ->
  GetQualificationScore
mkGetQualificationScore pQualificationTypeId_ pWorkerId_ =
  GetQualificationScore'
    { qualificationTypeId =
        pQualificationTypeId_,
      workerId = pWorkerId_
    }

-- | The ID of the QualificationType.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqsQualificationTypeId :: Lens.Lens' GetQualificationScore Lude.Text
gqsQualificationTypeId = Lens.lens (qualificationTypeId :: GetQualificationScore -> Lude.Text) (\s a -> s {qualificationTypeId = a} :: GetQualificationScore)
{-# DEPRECATED gqsQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The ID of the Worker whose Qualification is being updated.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqsWorkerId :: Lens.Lens' GetQualificationScore Lude.Text
gqsWorkerId = Lens.lens (workerId :: GetQualificationScore -> Lude.Text) (\s a -> s {workerId = a} :: GetQualificationScore)
{-# DEPRECATED gqsWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Lude.AWSRequest GetQualificationScore where
  type Rs GetQualificationScore = GetQualificationScoreResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetQualificationScoreResponse'
            Lude.<$> (x Lude..?> "Qualification")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetQualificationScore where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.GetQualificationScore" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetQualificationScore where
  toJSON GetQualificationScore' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("QualificationTypeId" Lude..= qualificationTypeId),
            Lude.Just ("WorkerId" Lude..= workerId)
          ]
      )

instance Lude.ToPath GetQualificationScore where
  toPath = Lude.const "/"

instance Lude.ToQuery GetQualificationScore where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetQualificationScoreResponse' smart constructor.
data GetQualificationScoreResponse = GetQualificationScoreResponse'
  { qualification ::
      Lude.Maybe Qualification,
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

-- | Creates a value of 'GetQualificationScoreResponse' with the minimum fields required to make a request.
--
-- * 'qualification' - The Qualification data structure of the Qualification assigned to a user, including the Qualification type and the value (score).
-- * 'responseStatus' - The response status code.
mkGetQualificationScoreResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetQualificationScoreResponse
mkGetQualificationScoreResponse pResponseStatus_ =
  GetQualificationScoreResponse'
    { qualification = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Qualification data structure of the Qualification assigned to a user, including the Qualification type and the value (score).
--
-- /Note:/ Consider using 'qualification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqsrsQualification :: Lens.Lens' GetQualificationScoreResponse (Lude.Maybe Qualification)
gqsrsQualification = Lens.lens (qualification :: GetQualificationScoreResponse -> Lude.Maybe Qualification) (\s a -> s {qualification = a} :: GetQualificationScoreResponse)
{-# DEPRECATED gqsrsQualification "Use generic-lens or generic-optics with 'qualification' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gqsrsResponseStatus :: Lens.Lens' GetQualificationScoreResponse Lude.Int
gqsrsResponseStatus = Lens.lens (responseStatus :: GetQualificationScoreResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetQualificationScoreResponse)
{-# DEPRECATED gqsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
