{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a specific conference provider.
module Network.AWS.AlexaBusiness.GetConferenceProvider
  ( -- * Creating a request
    GetConferenceProvider (..),
    mkGetConferenceProvider,

    -- ** Request lenses
    gcpConferenceProviderARN,

    -- * Destructuring the response
    GetConferenceProviderResponse (..),
    mkGetConferenceProviderResponse,

    -- ** Response lenses
    grsConferenceProvider,
    grsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetConferenceProvider' smart constructor.
newtype GetConferenceProvider = GetConferenceProvider'
  { conferenceProviderARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetConferenceProvider' with the minimum fields required to make a request.
--
-- * 'conferenceProviderARN' - The ARN of the newly created conference provider.
mkGetConferenceProvider ::
  -- | 'conferenceProviderARN'
  Lude.Text ->
  GetConferenceProvider
mkGetConferenceProvider pConferenceProviderARN_ =
  GetConferenceProvider'
    { conferenceProviderARN =
        pConferenceProviderARN_
    }

-- | The ARN of the newly created conference provider.
--
-- /Note:/ Consider using 'conferenceProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpConferenceProviderARN :: Lens.Lens' GetConferenceProvider Lude.Text
gcpConferenceProviderARN = Lens.lens (conferenceProviderARN :: GetConferenceProvider -> Lude.Text) (\s a -> s {conferenceProviderARN = a} :: GetConferenceProvider)
{-# DEPRECATED gcpConferenceProviderARN "Use generic-lens or generic-optics with 'conferenceProviderARN' instead." #-}

instance Lude.AWSRequest GetConferenceProvider where
  type Rs GetConferenceProvider = GetConferenceProviderResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetConferenceProviderResponse'
            Lude.<$> (x Lude..?> "ConferenceProvider")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetConferenceProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetConferenceProvider" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetConferenceProvider where
  toJSON GetConferenceProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("ConferenceProviderArn" Lude..= conferenceProviderARN)
          ]
      )

instance Lude.ToPath GetConferenceProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery GetConferenceProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetConferenceProviderResponse' smart constructor.
data GetConferenceProviderResponse = GetConferenceProviderResponse'
  { conferenceProvider ::
      Lude.Maybe ConferenceProvider,
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

-- | Creates a value of 'GetConferenceProviderResponse' with the minimum fields required to make a request.
--
-- * 'conferenceProvider' - The conference provider.
-- * 'responseStatus' - The response status code.
mkGetConferenceProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetConferenceProviderResponse
mkGetConferenceProviderResponse pResponseStatus_ =
  GetConferenceProviderResponse'
    { conferenceProvider = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The conference provider.
--
-- /Note:/ Consider using 'conferenceProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsConferenceProvider :: Lens.Lens' GetConferenceProviderResponse (Lude.Maybe ConferenceProvider)
grsConferenceProvider = Lens.lens (conferenceProvider :: GetConferenceProviderResponse -> Lude.Maybe ConferenceProvider) (\s a -> s {conferenceProvider = a} :: GetConferenceProviderResponse)
{-# DEPRECATED grsConferenceProvider "Use generic-lens or generic-optics with 'conferenceProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetConferenceProviderResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetConferenceProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetConferenceProviderResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
