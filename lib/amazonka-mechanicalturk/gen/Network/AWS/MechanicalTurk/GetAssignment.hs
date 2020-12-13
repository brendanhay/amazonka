{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetAssignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetAssignment@ operation retrieves the details of the specified Assignment.
module Network.AWS.MechanicalTurk.GetAssignment
  ( -- * Creating a request
    GetAssignment (..),
    mkGetAssignment,

    -- ** Request lenses
    gaAssignmentId,

    -- * Destructuring the response
    GetAssignmentResponse (..),
    mkGetAssignmentResponse,

    -- ** Response lenses
    garsHIT,
    garsAssignment,
    garsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAssignment' smart constructor.
newtype GetAssignment = GetAssignment'
  { -- | The ID of the Assignment to be retrieved.
    assignmentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAssignment' with the minimum fields required to make a request.
--
-- * 'assignmentId' - The ID of the Assignment to be retrieved.
mkGetAssignment ::
  -- | 'assignmentId'
  Lude.Text ->
  GetAssignment
mkGetAssignment pAssignmentId_ =
  GetAssignment' {assignmentId = pAssignmentId_}

-- | The ID of the Assignment to be retrieved.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAssignmentId :: Lens.Lens' GetAssignment Lude.Text
gaAssignmentId = Lens.lens (assignmentId :: GetAssignment -> Lude.Text) (\s a -> s {assignmentId = a} :: GetAssignment)
{-# DEPRECATED gaAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

instance Lude.AWSRequest GetAssignment where
  type Rs GetAssignment = GetAssignmentResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAssignmentResponse'
            Lude.<$> (x Lude..?> "HIT")
            Lude.<*> (x Lude..?> "Assignment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAssignment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.GetAssignment" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAssignment where
  toJSON GetAssignment' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AssignmentId" Lude..= assignmentId)])

instance Lude.ToPath GetAssignment where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAssignment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAssignmentResponse' smart constructor.
data GetAssignmentResponse = GetAssignmentResponse'
  { -- | The HIT associated with this assignment. The response includes one HIT element.
    hIT :: Lude.Maybe HIT,
    -- | The assignment. The response includes one Assignment element.
    assignment :: Lude.Maybe Assignment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAssignmentResponse' with the minimum fields required to make a request.
--
-- * 'hIT' - The HIT associated with this assignment. The response includes one HIT element.
-- * 'assignment' - The assignment. The response includes one Assignment element.
-- * 'responseStatus' - The response status code.
mkGetAssignmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAssignmentResponse
mkGetAssignmentResponse pResponseStatus_ =
  GetAssignmentResponse'
    { hIT = Lude.Nothing,
      assignment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The HIT associated with this assignment. The response includes one HIT element.
--
-- /Note:/ Consider using 'hIT' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsHIT :: Lens.Lens' GetAssignmentResponse (Lude.Maybe HIT)
garsHIT = Lens.lens (hIT :: GetAssignmentResponse -> Lude.Maybe HIT) (\s a -> s {hIT = a} :: GetAssignmentResponse)
{-# DEPRECATED garsHIT "Use generic-lens or generic-optics with 'hIT' instead." #-}

-- | The assignment. The response includes one Assignment element.
--
-- /Note:/ Consider using 'assignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsAssignment :: Lens.Lens' GetAssignmentResponse (Lude.Maybe Assignment)
garsAssignment = Lens.lens (assignment :: GetAssignmentResponse -> Lude.Maybe Assignment) (\s a -> s {assignment = a} :: GetAssignmentResponse)
{-# DEPRECATED garsAssignment "Use generic-lens or generic-optics with 'assignment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GetAssignmentResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GetAssignmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAssignmentResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
