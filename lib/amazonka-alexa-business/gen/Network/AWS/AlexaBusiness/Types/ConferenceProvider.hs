-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ConferenceProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferenceProvider
  ( ConferenceProvider (..),

    -- * Smart constructor
    mkConferenceProvider,

    -- * Lenses
    cpMeetingSetting,
    cpARN,
    cpPSTNDialIn,
    cpName,
    cpType,
    cpIPDialIn,
  )
where

import Network.AWS.AlexaBusiness.Types.ConferenceProviderType
import Network.AWS.AlexaBusiness.Types.IPDialIn
import Network.AWS.AlexaBusiness.Types.MeetingSetting
import Network.AWS.AlexaBusiness.Types.PSTNDialIn
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An entity that provides a conferencing solution. Alexa for Business acts as the voice interface and mediator that connects users to their preferred conference provider. Examples of conference providers include Amazon Chime, Zoom, Cisco, and Polycom.
--
-- /See:/ 'mkConferenceProvider' smart constructor.
data ConferenceProvider = ConferenceProvider'
  { meetingSetting ::
      Lude.Maybe MeetingSetting,
    arn :: Lude.Maybe Lude.Text,
    pSTNDialIn :: Lude.Maybe PSTNDialIn,
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ConferenceProviderType,
    ipDialIn :: Lude.Maybe IPDialIn
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConferenceProvider' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the newly created conference provider.
-- * 'ipDialIn' - The IP endpoint and protocol for calling.
-- * 'meetingSetting' - The meeting settings for the conference provider.
-- * 'name' - The name of the conference provider.
-- * 'pSTNDialIn' - The information for PSTN conferencing.
-- * 'type'' - The type of conference providers.
mkConferenceProvider ::
  ConferenceProvider
mkConferenceProvider =
  ConferenceProvider'
    { meetingSetting = Lude.Nothing,
      arn = Lude.Nothing,
      pSTNDialIn = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      ipDialIn = Lude.Nothing
    }

-- | The meeting settings for the conference provider.
--
-- /Note:/ Consider using 'meetingSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpMeetingSetting :: Lens.Lens' ConferenceProvider (Lude.Maybe MeetingSetting)
cpMeetingSetting = Lens.lens (meetingSetting :: ConferenceProvider -> Lude.Maybe MeetingSetting) (\s a -> s {meetingSetting = a} :: ConferenceProvider)
{-# DEPRECATED cpMeetingSetting "Use generic-lens or generic-optics with 'meetingSetting' instead." #-}

-- | The ARN of the newly created conference provider.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpARN :: Lens.Lens' ConferenceProvider (Lude.Maybe Lude.Text)
cpARN = Lens.lens (arn :: ConferenceProvider -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ConferenceProvider)
{-# DEPRECATED cpARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The information for PSTN conferencing.
--
-- /Note:/ Consider using 'pSTNDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPSTNDialIn :: Lens.Lens' ConferenceProvider (Lude.Maybe PSTNDialIn)
cpPSTNDialIn = Lens.lens (pSTNDialIn :: ConferenceProvider -> Lude.Maybe PSTNDialIn) (\s a -> s {pSTNDialIn = a} :: ConferenceProvider)
{-# DEPRECATED cpPSTNDialIn "Use generic-lens or generic-optics with 'pSTNDialIn' instead." #-}

-- | The name of the conference provider.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' ConferenceProvider (Lude.Maybe Lude.Text)
cpName = Lens.lens (name :: ConferenceProvider -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ConferenceProvider)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of conference providers.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpType :: Lens.Lens' ConferenceProvider (Lude.Maybe ConferenceProviderType)
cpType = Lens.lens (type' :: ConferenceProvider -> Lude.Maybe ConferenceProviderType) (\s a -> s {type' = a} :: ConferenceProvider)
{-# DEPRECATED cpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The IP endpoint and protocol for calling.
--
-- /Note:/ Consider using 'ipDialIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpIPDialIn :: Lens.Lens' ConferenceProvider (Lude.Maybe IPDialIn)
cpIPDialIn = Lens.lens (ipDialIn :: ConferenceProvider -> Lude.Maybe IPDialIn) (\s a -> s {ipDialIn = a} :: ConferenceProvider)
{-# DEPRECATED cpIPDialIn "Use generic-lens or generic-optics with 'ipDialIn' instead." #-}

instance Lude.FromJSON ConferenceProvider where
  parseJSON =
    Lude.withObject
      "ConferenceProvider"
      ( \x ->
          ConferenceProvider'
            Lude.<$> (x Lude..:? "MeetingSetting")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "PSTNDialIn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "IPDialIn")
      )
