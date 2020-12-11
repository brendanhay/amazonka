-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ConferencePreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferencePreference
  ( ConferencePreference (..),

    -- * Smart constructor
    mkConferencePreference,

    -- * Lenses
    cpDefaultConferenceProviderARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The default conference provider that is used if no other scheduled meetings are detected.
--
-- /See:/ 'mkConferencePreference' smart constructor.
newtype ConferencePreference = ConferencePreference'
  { defaultConferenceProviderARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConferencePreference' with the minimum fields required to make a request.
--
-- * 'defaultConferenceProviderARN' - The ARN of the default conference provider.
mkConferencePreference ::
  ConferencePreference
mkConferencePreference =
  ConferencePreference'
    { defaultConferenceProviderARN =
        Lude.Nothing
    }

-- | The ARN of the default conference provider.
--
-- /Note:/ Consider using 'defaultConferenceProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDefaultConferenceProviderARN :: Lens.Lens' ConferencePreference (Lude.Maybe Lude.Text)
cpDefaultConferenceProviderARN = Lens.lens (defaultConferenceProviderARN :: ConferencePreference -> Lude.Maybe Lude.Text) (\s a -> s {defaultConferenceProviderARN = a} :: ConferencePreference)
{-# DEPRECATED cpDefaultConferenceProviderARN "Use generic-lens or generic-optics with 'defaultConferenceProviderARN' instead." #-}

instance Lude.FromJSON ConferencePreference where
  parseJSON =
    Lude.withObject
      "ConferencePreference"
      ( \x ->
          ConferencePreference'
            Lude.<$> (x Lude..:? "DefaultConferenceProviderArn")
      )

instance Lude.ToJSON ConferencePreference where
  toJSON ConferencePreference' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultConferenceProviderArn" Lude..=)
              Lude.<$> defaultConferenceProviderARN
          ]
      )
