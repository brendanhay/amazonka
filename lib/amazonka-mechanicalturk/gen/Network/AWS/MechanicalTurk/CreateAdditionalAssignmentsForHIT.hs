{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateAdditionalAssignmentsForHIT@ operation increases the maximum number of assignments of an existing HIT.
--
-- To extend the maximum number of assignments, specify the number of additional assignments.
module Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT
  ( -- * Creating a request
    CreateAdditionalAssignmentsForHIT (..),
    mkCreateAdditionalAssignmentsForHIT,

    -- ** Request lenses
    caafhitUniqueRequestToken,
    caafhitHITId,
    caafhitNumberOfAdditionalAssignments,

    -- * Destructuring the response
    CreateAdditionalAssignmentsForHITResponse (..),
    mkCreateAdditionalAssignmentsForHITResponse,

    -- ** Response lenses
    caafhitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateAdditionalAssignmentsForHIT' smart constructor.
data CreateAdditionalAssignmentsForHIT = CreateAdditionalAssignmentsForHIT'
  { uniqueRequestToken ::
      Lude.Maybe Lude.Text,
    hITId :: Lude.Text,
    numberOfAdditionalAssignments ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAdditionalAssignmentsForHIT' with the minimum fields required to make a request.
--
-- * 'hITId' - The ID of the HIT to extend.
-- * 'numberOfAdditionalAssignments' - The number of additional assignments to request for this HIT.
-- * 'uniqueRequestToken' - A unique identifier for this request, which allows you to retry the call on error without extending the HIT multiple times. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the extend HIT already exists in the system from a previous call using the same @UniqueRequestToken@ , subsequent calls will return an error with a message containing the request ID.
mkCreateAdditionalAssignmentsForHIT ::
  -- | 'hITId'
  Lude.Text ->
  -- | 'numberOfAdditionalAssignments'
  Lude.Int ->
  CreateAdditionalAssignmentsForHIT
mkCreateAdditionalAssignmentsForHIT
  pHITId_
  pNumberOfAdditionalAssignments_ =
    CreateAdditionalAssignmentsForHIT'
      { uniqueRequestToken =
          Lude.Nothing,
        hITId = pHITId_,
        numberOfAdditionalAssignments =
          pNumberOfAdditionalAssignments_
      }

-- | A unique identifier for this request, which allows you to retry the call on error without extending the HIT multiple times. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the extend HIT already exists in the system from a previous call using the same @UniqueRequestToken@ , subsequent calls will return an error with a message containing the request ID.
--
-- /Note:/ Consider using 'uniqueRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caafhitUniqueRequestToken :: Lens.Lens' CreateAdditionalAssignmentsForHIT (Lude.Maybe Lude.Text)
caafhitUniqueRequestToken = Lens.lens (uniqueRequestToken :: CreateAdditionalAssignmentsForHIT -> Lude.Maybe Lude.Text) (\s a -> s {uniqueRequestToken = a} :: CreateAdditionalAssignmentsForHIT)
{-# DEPRECATED caafhitUniqueRequestToken "Use generic-lens or generic-optics with 'uniqueRequestToken' instead." #-}

-- | The ID of the HIT to extend.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caafhitHITId :: Lens.Lens' CreateAdditionalAssignmentsForHIT Lude.Text
caafhitHITId = Lens.lens (hITId :: CreateAdditionalAssignmentsForHIT -> Lude.Text) (\s a -> s {hITId = a} :: CreateAdditionalAssignmentsForHIT)
{-# DEPRECATED caafhitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The number of additional assignments to request for this HIT.
--
-- /Note:/ Consider using 'numberOfAdditionalAssignments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caafhitNumberOfAdditionalAssignments :: Lens.Lens' CreateAdditionalAssignmentsForHIT Lude.Int
caafhitNumberOfAdditionalAssignments = Lens.lens (numberOfAdditionalAssignments :: CreateAdditionalAssignmentsForHIT -> Lude.Int) (\s a -> s {numberOfAdditionalAssignments = a} :: CreateAdditionalAssignmentsForHIT)
{-# DEPRECATED caafhitNumberOfAdditionalAssignments "Use generic-lens or generic-optics with 'numberOfAdditionalAssignments' instead." #-}

instance Lude.AWSRequest CreateAdditionalAssignmentsForHIT where
  type
    Rs CreateAdditionalAssignmentsForHIT =
      CreateAdditionalAssignmentsForHITResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateAdditionalAssignmentsForHITResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAdditionalAssignmentsForHIT where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.CreateAdditionalAssignmentsForHIT" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAdditionalAssignmentsForHIT where
  toJSON CreateAdditionalAssignmentsForHIT' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UniqueRequestToken" Lude..=) Lude.<$> uniqueRequestToken,
            Lude.Just ("HITId" Lude..= hITId),
            Lude.Just
              ( "NumberOfAdditionalAssignments"
                  Lude..= numberOfAdditionalAssignments
              )
          ]
      )

instance Lude.ToPath CreateAdditionalAssignmentsForHIT where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAdditionalAssignmentsForHIT where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAdditionalAssignmentsForHITResponse' smart constructor.
newtype CreateAdditionalAssignmentsForHITResponse = CreateAdditionalAssignmentsForHITResponse'
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

-- | Creates a value of 'CreateAdditionalAssignmentsForHITResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateAdditionalAssignmentsForHITResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAdditionalAssignmentsForHITResponse
mkCreateAdditionalAssignmentsForHITResponse pResponseStatus_ =
  CreateAdditionalAssignmentsForHITResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caafhitrsResponseStatus :: Lens.Lens' CreateAdditionalAssignmentsForHITResponse Lude.Int
caafhitrsResponseStatus = Lens.lens (responseStatus :: CreateAdditionalAssignmentsForHITResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAdditionalAssignmentsForHITResponse)
{-# DEPRECATED caafhitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
