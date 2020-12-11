{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing conference provider's settings.
module Network.AWS.AlexaBusiness.UpdateConferenceProvider
  ( -- * Creating a request
    UpdateConferenceProvider (..),
    mkUpdateConferenceProvider,

    -- ** Request lenses
    ucpPSTNDialIn,
    ucpIPDialIn,
    ucpConferenceProviderARN,
    ucpConferenceProviderType,
    ucpMeetingSetting,

    -- * Destructuring the response
    UpdateConferenceProviderResponse (..),
    mkUpdateConferenceProviderResponse,

    -- ** Response lenses
    ucprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateConferenceProvider' smart constructor.
data UpdateConferenceProvider = UpdateConferenceProvider'
  { pSTNDialIn ::
      Lude.Maybe PSTNDialIn,
    ipDialIn :: Lude.Maybe IPDialIn,
    conferenceProviderARN :: Lude.Text,
    conferenceProviderType ::
      ConferenceProviderType,
    meetingSetting :: MeetingSetting
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConferenceProvider' with the minimum fields required to make a request.
--
-- * 'conferenceProviderARN' - The ARN of the conference provider.
-- * 'conferenceProviderType' - The type of the conference provider.
-- * 'ipDialIn' - The IP endpoint and protocol for calling.
-- * 'meetingSetting' - The meeting settings for the conference provider.
-- * 'pSTNDialIn' - The information for PSTN conferencing.
mkUpdateConferenceProvider ::
  -- | 'conferenceProviderARN'
  Lude.Text ->
  -- | 'conferenceProviderType'
  ConferenceProviderType ->
  -- | 'meetingSetting'
  MeetingSetting ->
  UpdateConferenceProvider
mkUpdateConferenceProvider
  pConferenceProviderARN_
  pConferenceProviderType_
  pMeetingSetting_ =
    UpdateConferenceProvider'
      { pSTNDialIn = Lude.Nothing,
        ipDialIn = Lude.Nothing,
        conferenceProviderARN = pConferenceProviderARN_,
        conferenceProviderType = pConferenceProviderType_,
        meetingSetting = pMeetingSetting_
      }

-- | The information for PSTN conferencing.
--
-- /Note:/ Consider using 'pSTNDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpPSTNDialIn :: Lens.Lens' UpdateConferenceProvider (Lude.Maybe PSTNDialIn)
ucpPSTNDialIn = Lens.lens (pSTNDialIn :: UpdateConferenceProvider -> Lude.Maybe PSTNDialIn) (\s a -> s {pSTNDialIn = a} :: UpdateConferenceProvider)
{-# DEPRECATED ucpPSTNDialIn "Use generic-lens or generic-optics with 'pSTNDialIn' instead." #-}

-- | The IP endpoint and protocol for calling.
--
-- /Note:/ Consider using 'ipDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpIPDialIn :: Lens.Lens' UpdateConferenceProvider (Lude.Maybe IPDialIn)
ucpIPDialIn = Lens.lens (ipDialIn :: UpdateConferenceProvider -> Lude.Maybe IPDialIn) (\s a -> s {ipDialIn = a} :: UpdateConferenceProvider)
{-# DEPRECATED ucpIPDialIn "Use generic-lens or generic-optics with 'ipDialIn' instead." #-}

-- | The ARN of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpConferenceProviderARN :: Lens.Lens' UpdateConferenceProvider Lude.Text
ucpConferenceProviderARN = Lens.lens (conferenceProviderARN :: UpdateConferenceProvider -> Lude.Text) (\s a -> s {conferenceProviderARN = a} :: UpdateConferenceProvider)
{-# DEPRECATED ucpConferenceProviderARN "Use generic-lens or generic-optics with 'conferenceProviderARN' instead." #-}

-- | The type of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpConferenceProviderType :: Lens.Lens' UpdateConferenceProvider ConferenceProviderType
ucpConferenceProviderType = Lens.lens (conferenceProviderType :: UpdateConferenceProvider -> ConferenceProviderType) (\s a -> s {conferenceProviderType = a} :: UpdateConferenceProvider)
{-# DEPRECATED ucpConferenceProviderType "Use generic-lens or generic-optics with 'conferenceProviderType' instead." #-}

-- | The meeting settings for the conference provider.
--
-- /Note:/ Consider using 'meetingSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucpMeetingSetting :: Lens.Lens' UpdateConferenceProvider MeetingSetting
ucpMeetingSetting = Lens.lens (meetingSetting :: UpdateConferenceProvider -> MeetingSetting) (\s a -> s {meetingSetting = a} :: UpdateConferenceProvider)
{-# DEPRECATED ucpMeetingSetting "Use generic-lens or generic-optics with 'meetingSetting' instead." #-}

instance Lude.AWSRequest UpdateConferenceProvider where
  type Rs UpdateConferenceProvider = UpdateConferenceProviderResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateConferenceProviderResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConferenceProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateConferenceProvider" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateConferenceProvider where
  toJSON UpdateConferenceProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PSTNDialIn" Lude..=) Lude.<$> pSTNDialIn,
            ("IPDialIn" Lude..=) Lude.<$> ipDialIn,
            Lude.Just ("ConferenceProviderArn" Lude..= conferenceProviderARN),
            Lude.Just
              ("ConferenceProviderType" Lude..= conferenceProviderType),
            Lude.Just ("MeetingSetting" Lude..= meetingSetting)
          ]
      )

instance Lude.ToPath UpdateConferenceProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateConferenceProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateConferenceProviderResponse' smart constructor.
newtype UpdateConferenceProviderResponse = UpdateConferenceProviderResponse'
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

-- | Creates a value of 'UpdateConferenceProviderResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateConferenceProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConferenceProviderResponse
mkUpdateConferenceProviderResponse pResponseStatus_ =
  UpdateConferenceProviderResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucprsResponseStatus :: Lens.Lens' UpdateConferenceProviderResponse Lude.Int
ucprsResponseStatus = Lens.lens (responseStatus :: UpdateConferenceProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConferenceProviderResponse)
{-# DEPRECATED ucprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
