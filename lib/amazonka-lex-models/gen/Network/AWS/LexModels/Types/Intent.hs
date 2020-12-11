-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Intent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Intent
  ( Intent (..),

    -- * Smart constructor
    mkIntent,

    -- * Lenses
    iIntentName,
    iIntentVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the specific version of an intent.
--
-- /See:/ 'mkIntent' smart constructor.
data Intent = Intent'
  { intentName :: Lude.Text,
    intentVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Intent' with the minimum fields required to make a request.
--
-- * 'intentName' - The name of the intent.
-- * 'intentVersion' - The version of the intent.
mkIntent ::
  -- | 'intentName'
  Lude.Text ->
  -- | 'intentVersion'
  Lude.Text ->
  Intent
mkIntent pIntentName_ pIntentVersion_ =
  Intent'
    { intentName = pIntentName_,
      intentVersion = pIntentVersion_
    }

-- | The name of the intent.
--
-- /Note:/ Consider using 'intentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIntentName :: Lens.Lens' Intent Lude.Text
iIntentName = Lens.lens (intentName :: Intent -> Lude.Text) (\s a -> s {intentName = a} :: Intent)
{-# DEPRECATED iIntentName "Use generic-lens or generic-optics with 'intentName' instead." #-}

-- | The version of the intent.
--
-- /Note:/ Consider using 'intentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIntentVersion :: Lens.Lens' Intent Lude.Text
iIntentVersion = Lens.lens (intentVersion :: Intent -> Lude.Text) (\s a -> s {intentVersion = a} :: Intent)
{-# DEPRECATED iIntentVersion "Use generic-lens or generic-optics with 'intentVersion' instead." #-}

instance Lude.FromJSON Intent where
  parseJSON =
    Lude.withObject
      "Intent"
      ( \x ->
          Intent'
            Lude.<$> (x Lude..: "intentName") Lude.<*> (x Lude..: "intentVersion")
      )

instance Lude.ToJSON Intent where
  toJSON Intent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("intentName" Lude..= intentName),
            Lude.Just ("intentVersion" Lude..= intentVersion)
          ]
      )
