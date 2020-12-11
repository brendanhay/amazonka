{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetConferencePreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the existing conference preferences.
module Network.AWS.AlexaBusiness.GetConferencePreference
  ( -- * Creating a request
    GetConferencePreference (..),
    mkGetConferencePreference,

    -- * Destructuring the response
    GetConferencePreferenceResponse (..),
    mkGetConferencePreferenceResponse,

    -- ** Response lenses
    gcprsPreference,
    gcprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConferencePreference' smart constructor.
data GetConferencePreference = GetConferencePreference'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConferencePreference' with the minimum fields required to make a request.
mkGetConferencePreference ::
  GetConferencePreference
mkGetConferencePreference = GetConferencePreference'

instance Lude.AWSRequest GetConferencePreference where
  type Rs GetConferencePreference = GetConferencePreferenceResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConferencePreferenceResponse'
            Lude.<$> (x Lude..?> "Preference") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConferencePreference where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetConferencePreference" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetConferencePreference where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetConferencePreference where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConferencePreference where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConferencePreferenceResponse' smart constructor.
data GetConferencePreferenceResponse = GetConferencePreferenceResponse'
  { preference ::
      Lude.Maybe
        ConferencePreference,
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

-- | Creates a value of 'GetConferencePreferenceResponse' with the minimum fields required to make a request.
--
-- * 'preference' - The conference preference.
-- * 'responseStatus' - The response status code.
mkGetConferencePreferenceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConferencePreferenceResponse
mkGetConferencePreferenceResponse pResponseStatus_ =
  GetConferencePreferenceResponse'
    { preference = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The conference preference.
--
-- /Note:/ Consider using 'preference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprsPreference :: Lens.Lens' GetConferencePreferenceResponse (Lude.Maybe ConferencePreference)
gcprsPreference = Lens.lens (preference :: GetConferencePreferenceResponse -> Lude.Maybe ConferencePreference) (\s a -> s {preference = a} :: GetConferencePreferenceResponse)
{-# DEPRECATED gcprsPreference "Use generic-lens or generic-optics with 'preference' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcprsResponseStatus :: Lens.Lens' GetConferencePreferenceResponse Lude.Int
gcprsResponseStatus = Lens.lens (responseStatus :: GetConferencePreferenceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConferencePreferenceResponse)
{-# DEPRECATED gcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
