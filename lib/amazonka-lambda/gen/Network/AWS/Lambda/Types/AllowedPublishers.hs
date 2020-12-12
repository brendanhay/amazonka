{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AllowedPublishers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AllowedPublishers
  ( AllowedPublishers (..),

    -- * Smart constructor
    mkAllowedPublishers,

    -- * Lenses
    apSigningProfileVersionARNs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | List of signing profiles that can sign a code package.
--
-- /See:/ 'mkAllowedPublishers' smart constructor.
newtype AllowedPublishers = AllowedPublishers'
  { signingProfileVersionARNs ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllowedPublishers' with the minimum fields required to make a request.
--
-- * 'signingProfileVersionARNs' - The Amazon Resource Name (ARN) for each of the signing profiles. A signing profile defines a trusted user who can sign a code package.
mkAllowedPublishers ::
  -- | 'signingProfileVersionARNs'
  Lude.NonEmpty Lude.Text ->
  AllowedPublishers
mkAllowedPublishers pSigningProfileVersionARNs_ =
  AllowedPublishers'
    { signingProfileVersionARNs =
        pSigningProfileVersionARNs_
    }

-- | The Amazon Resource Name (ARN) for each of the signing profiles. A signing profile defines a trusted user who can sign a code package.
--
-- /Note:/ Consider using 'signingProfileVersionARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSigningProfileVersionARNs :: Lens.Lens' AllowedPublishers (Lude.NonEmpty Lude.Text)
apSigningProfileVersionARNs = Lens.lens (signingProfileVersionARNs :: AllowedPublishers -> Lude.NonEmpty Lude.Text) (\s a -> s {signingProfileVersionARNs = a} :: AllowedPublishers)
{-# DEPRECATED apSigningProfileVersionARNs "Use generic-lens or generic-optics with 'signingProfileVersionARNs' instead." #-}

instance Lude.FromJSON AllowedPublishers where
  parseJSON =
    Lude.withObject
      "AllowedPublishers"
      ( \x ->
          AllowedPublishers'
            Lude.<$> (x Lude..: "SigningProfileVersionArns")
      )

instance Lude.ToJSON AllowedPublishers where
  toJSON AllowedPublishers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("SigningProfileVersionArns" Lude..= signingProfileVersionARNs)
          ]
      )
