{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.PutConferencePreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the conference preferences on a specific conference provider at the account level.
module Network.AWS.AlexaBusiness.PutConferencePreference
  ( -- * Creating a request
    PutConferencePreference (..),
    mkPutConferencePreference,

    -- ** Request lenses
    pcpConferencePreference,

    -- * Destructuring the response
    PutConferencePreferenceResponse (..),
    mkPutConferencePreferenceResponse,

    -- ** Response lenses
    pcprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutConferencePreference' smart constructor.
newtype PutConferencePreference = PutConferencePreference'
  { conferencePreference ::
      ConferencePreference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutConferencePreference' with the minimum fields required to make a request.
--
-- * 'conferencePreference' - The conference preference of a specific conference provider.
mkPutConferencePreference ::
  -- | 'conferencePreference'
  ConferencePreference ->
  PutConferencePreference
mkPutConferencePreference pConferencePreference_ =
  PutConferencePreference'
    { conferencePreference =
        pConferencePreference_
    }

-- | The conference preference of a specific conference provider.
--
-- /Note:/ Consider using 'conferencePreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcpConferencePreference :: Lens.Lens' PutConferencePreference ConferencePreference
pcpConferencePreference = Lens.lens (conferencePreference :: PutConferencePreference -> ConferencePreference) (\s a -> s {conferencePreference = a} :: PutConferencePreference)
{-# DEPRECATED pcpConferencePreference "Use generic-lens or generic-optics with 'conferencePreference' instead." #-}

instance Lude.AWSRequest PutConferencePreference where
  type Rs PutConferencePreference = PutConferencePreferenceResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutConferencePreferenceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutConferencePreference where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.PutConferencePreference" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutConferencePreference where
  toJSON PutConferencePreference' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ConferencePreference" Lude..= conferencePreference)]
      )

instance Lude.ToPath PutConferencePreference where
  toPath = Lude.const "/"

instance Lude.ToQuery PutConferencePreference where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutConferencePreferenceResponse' smart constructor.
newtype PutConferencePreferenceResponse = PutConferencePreferenceResponse'
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

-- | Creates a value of 'PutConferencePreferenceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutConferencePreferenceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutConferencePreferenceResponse
mkPutConferencePreferenceResponse pResponseStatus_ =
  PutConferencePreferenceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcprsResponseStatus :: Lens.Lens' PutConferencePreferenceResponse Lude.Int
pcprsResponseStatus = Lens.lens (responseStatus :: PutConferencePreferenceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutConferencePreferenceResponse)
{-# DEPRECATED pcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
