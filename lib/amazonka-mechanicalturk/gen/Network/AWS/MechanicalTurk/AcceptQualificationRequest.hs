{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.AcceptQualificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AcceptQualificationRequest@ operation approves a Worker's request for a Qualification.
--
-- Only the owner of the Qualification type can grant a Qualification request for that type.
-- A successful request for the @AcceptQualificationRequest@ operation returns with no errors and an empty body.
module Network.AWS.MechanicalTurk.AcceptQualificationRequest
  ( -- * Creating a request
    AcceptQualificationRequest (..),
    mkAcceptQualificationRequest,

    -- ** Request lenses
    aqrIntegerValue,
    aqrQualificationRequestId,

    -- * Destructuring the response
    AcceptQualificationRequestResponse (..),
    mkAcceptQualificationRequestResponse,

    -- ** Response lenses
    aqrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAcceptQualificationRequest' smart constructor.
data AcceptQualificationRequest = AcceptQualificationRequest'
  { integerValue ::
      Lude.Maybe Lude.Int,
    qualificationRequestId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptQualificationRequest' with the minimum fields required to make a request.
--
-- * 'integerValue' - The value of the Qualification. You can omit this value if you are using the presence or absence of the Qualification as the basis for a HIT requirement.
-- * 'qualificationRequestId' - The ID of the Qualification request, as returned by the @GetQualificationRequests@ operation.
mkAcceptQualificationRequest ::
  -- | 'qualificationRequestId'
  Lude.Text ->
  AcceptQualificationRequest
mkAcceptQualificationRequest pQualificationRequestId_ =
  AcceptQualificationRequest'
    { integerValue = Lude.Nothing,
      qualificationRequestId = pQualificationRequestId_
    }

-- | The value of the Qualification. You can omit this value if you are using the presence or absence of the Qualification as the basis for a HIT requirement.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqrIntegerValue :: Lens.Lens' AcceptQualificationRequest (Lude.Maybe Lude.Int)
aqrIntegerValue = Lens.lens (integerValue :: AcceptQualificationRequest -> Lude.Maybe Lude.Int) (\s a -> s {integerValue = a} :: AcceptQualificationRequest)
{-# DEPRECATED aqrIntegerValue "Use generic-lens or generic-optics with 'integerValue' instead." #-}

-- | The ID of the Qualification request, as returned by the @GetQualificationRequests@ operation.
--
-- /Note:/ Consider using 'qualificationRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqrQualificationRequestId :: Lens.Lens' AcceptQualificationRequest Lude.Text
aqrQualificationRequestId = Lens.lens (qualificationRequestId :: AcceptQualificationRequest -> Lude.Text) (\s a -> s {qualificationRequestId = a} :: AcceptQualificationRequest)
{-# DEPRECATED aqrQualificationRequestId "Use generic-lens or generic-optics with 'qualificationRequestId' instead." #-}

instance Lude.AWSRequest AcceptQualificationRequest where
  type
    Rs AcceptQualificationRequest =
      AcceptQualificationRequestResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AcceptQualificationRequestResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptQualificationRequest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.AcceptQualificationRequest" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcceptQualificationRequest where
  toJSON AcceptQualificationRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IntegerValue" Lude..=) Lude.<$> integerValue,
            Lude.Just
              ("QualificationRequestId" Lude..= qualificationRequestId)
          ]
      )

instance Lude.ToPath AcceptQualificationRequest where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptQualificationRequest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAcceptQualificationRequestResponse' smart constructor.
newtype AcceptQualificationRequestResponse = AcceptQualificationRequestResponse'
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

-- | Creates a value of 'AcceptQualificationRequestResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAcceptQualificationRequestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptQualificationRequestResponse
mkAcceptQualificationRequestResponse pResponseStatus_ =
  AcceptQualificationRequestResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqrrsResponseStatus :: Lens.Lens' AcceptQualificationRequestResponse Lude.Int
aqrrsResponseStatus = Lens.lens (responseStatus :: AcceptQualificationRequestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptQualificationRequestResponse)
{-# DEPRECATED aqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
