{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new conference provider under the user's AWS account.
module Network.AWS.AlexaBusiness.CreateConferenceProvider
  ( -- * Creating a request
    CreateConferenceProvider (..),
    mkCreateConferenceProvider,

    -- ** Request lenses
    ccpPSTNDialIn,
    ccpClientRequestToken,
    ccpIPDialIn,
    ccpConferenceProviderName,
    ccpConferenceProviderType,
    ccpMeetingSetting,

    -- * Destructuring the response
    CreateConferenceProviderResponse (..),
    mkCreateConferenceProviderResponse,

    -- ** Response lenses
    ccprsConferenceProviderARN,
    ccprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateConferenceProvider' smart constructor.
data CreateConferenceProvider = CreateConferenceProvider'
  { pSTNDialIn ::
      Lude.Maybe PSTNDialIn,
    clientRequestToken ::
      Lude.Maybe Lude.Text,
    ipDialIn :: Lude.Maybe IPDialIn,
    conferenceProviderName :: Lude.Text,
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

-- | Creates a value of 'CreateConferenceProvider' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - The request token of the client.
-- * 'conferenceProviderName' - The name of the conference provider.
-- * 'conferenceProviderType' - Represents a type within a list of predefined types.
-- * 'ipDialIn' - The IP endpoint and protocol for calling.
-- * 'meetingSetting' - The meeting settings for the conference provider.
-- * 'pSTNDialIn' - The information for PSTN conferencing.
mkCreateConferenceProvider ::
  -- | 'conferenceProviderName'
  Lude.Text ->
  -- | 'conferenceProviderType'
  ConferenceProviderType ->
  -- | 'meetingSetting'
  MeetingSetting ->
  CreateConferenceProvider
mkCreateConferenceProvider
  pConferenceProviderName_
  pConferenceProviderType_
  pMeetingSetting_ =
    CreateConferenceProvider'
      { pSTNDialIn = Lude.Nothing,
        clientRequestToken = Lude.Nothing,
        ipDialIn = Lude.Nothing,
        conferenceProviderName = pConferenceProviderName_,
        conferenceProviderType = pConferenceProviderType_,
        meetingSetting = pMeetingSetting_
      }

-- | The information for PSTN conferencing.
--
-- /Note:/ Consider using 'pSTNDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpPSTNDialIn :: Lens.Lens' CreateConferenceProvider (Lude.Maybe PSTNDialIn)
ccpPSTNDialIn = Lens.lens (pSTNDialIn :: CreateConferenceProvider -> Lude.Maybe PSTNDialIn) (\s a -> s {pSTNDialIn = a} :: CreateConferenceProvider)
{-# DEPRECATED ccpPSTNDialIn "Use generic-lens or generic-optics with 'pSTNDialIn' instead." #-}

-- | The request token of the client.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpClientRequestToken :: Lens.Lens' CreateConferenceProvider (Lude.Maybe Lude.Text)
ccpClientRequestToken = Lens.lens (clientRequestToken :: CreateConferenceProvider -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateConferenceProvider)
{-# DEPRECATED ccpClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The IP endpoint and protocol for calling.
--
-- /Note:/ Consider using 'ipDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpIPDialIn :: Lens.Lens' CreateConferenceProvider (Lude.Maybe IPDialIn)
ccpIPDialIn = Lens.lens (ipDialIn :: CreateConferenceProvider -> Lude.Maybe IPDialIn) (\s a -> s {ipDialIn = a} :: CreateConferenceProvider)
{-# DEPRECATED ccpIPDialIn "Use generic-lens or generic-optics with 'ipDialIn' instead." #-}

-- | The name of the conference provider.
--
-- /Note:/ Consider using 'conferenceProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpConferenceProviderName :: Lens.Lens' CreateConferenceProvider Lude.Text
ccpConferenceProviderName = Lens.lens (conferenceProviderName :: CreateConferenceProvider -> Lude.Text) (\s a -> s {conferenceProviderName = a} :: CreateConferenceProvider)
{-# DEPRECATED ccpConferenceProviderName "Use generic-lens or generic-optics with 'conferenceProviderName' instead." #-}

-- | Represents a type within a list of predefined types.
--
-- /Note:/ Consider using 'conferenceProviderType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpConferenceProviderType :: Lens.Lens' CreateConferenceProvider ConferenceProviderType
ccpConferenceProviderType = Lens.lens (conferenceProviderType :: CreateConferenceProvider -> ConferenceProviderType) (\s a -> s {conferenceProviderType = a} :: CreateConferenceProvider)
{-# DEPRECATED ccpConferenceProviderType "Use generic-lens or generic-optics with 'conferenceProviderType' instead." #-}

-- | The meeting settings for the conference provider.
--
-- /Note:/ Consider using 'meetingSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccpMeetingSetting :: Lens.Lens' CreateConferenceProvider MeetingSetting
ccpMeetingSetting = Lens.lens (meetingSetting :: CreateConferenceProvider -> MeetingSetting) (\s a -> s {meetingSetting = a} :: CreateConferenceProvider)
{-# DEPRECATED ccpMeetingSetting "Use generic-lens or generic-optics with 'meetingSetting' instead." #-}

instance Lude.AWSRequest CreateConferenceProvider where
  type Rs CreateConferenceProvider = CreateConferenceProviderResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateConferenceProviderResponse'
            Lude.<$> (x Lude..?> "ConferenceProviderArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConferenceProvider where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.CreateConferenceProvider" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateConferenceProvider where
  toJSON CreateConferenceProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PSTNDialIn" Lude..=) Lude.<$> pSTNDialIn,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("IPDialIn" Lude..=) Lude.<$> ipDialIn,
            Lude.Just
              ("ConferenceProviderName" Lude..= conferenceProviderName),
            Lude.Just
              ("ConferenceProviderType" Lude..= conferenceProviderType),
            Lude.Just ("MeetingSetting" Lude..= meetingSetting)
          ]
      )

instance Lude.ToPath CreateConferenceProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConferenceProvider where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateConferenceProviderResponse' smart constructor.
data CreateConferenceProviderResponse = CreateConferenceProviderResponse'
  { conferenceProviderARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
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

-- | Creates a value of 'CreateConferenceProviderResponse' with the minimum fields required to make a request.
--
-- * 'conferenceProviderARN' - The ARN of the newly-created conference provider.
-- * 'responseStatus' - The response status code.
mkCreateConferenceProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConferenceProviderResponse
mkCreateConferenceProviderResponse pResponseStatus_ =
  CreateConferenceProviderResponse'
    { conferenceProviderARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the newly-created conference provider.
--
-- /Note:/ Consider using 'conferenceProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprsConferenceProviderARN :: Lens.Lens' CreateConferenceProviderResponse (Lude.Maybe Lude.Text)
ccprsConferenceProviderARN = Lens.lens (conferenceProviderARN :: CreateConferenceProviderResponse -> Lude.Maybe Lude.Text) (\s a -> s {conferenceProviderARN = a} :: CreateConferenceProviderResponse)
{-# DEPRECATED ccprsConferenceProviderARN "Use generic-lens or generic-optics with 'conferenceProviderARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccprsResponseStatus :: Lens.Lens' CreateConferenceProviderResponse Lude.Int
ccprsResponseStatus = Lens.lens (responseStatus :: CreateConferenceProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConferenceProviderResponse)
{-# DEPRECATED ccprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
