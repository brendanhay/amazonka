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
    apSigningProfileVersionArns,
  )
where

import qualified Network.AWS.Lambda.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | List of signing profiles that can sign a code package.
--
-- /See:/ 'mkAllowedPublishers' smart constructor.
newtype AllowedPublishers = AllowedPublishers'
  { -- | The Amazon Resource Name (ARN) for each of the signing profiles. A signing profile defines a trusted user who can sign a code package.
    signingProfileVersionArns :: Core.NonEmpty Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AllowedPublishers' value with any optional fields omitted.
mkAllowedPublishers ::
  -- | 'signingProfileVersionArns'
  Core.NonEmpty Types.Arn ->
  AllowedPublishers
mkAllowedPublishers signingProfileVersionArns =
  AllowedPublishers' {signingProfileVersionArns}

-- | The Amazon Resource Name (ARN) for each of the signing profiles. A signing profile defines a trusted user who can sign a code package.
--
-- /Note:/ Consider using 'signingProfileVersionArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSigningProfileVersionArns :: Lens.Lens' AllowedPublishers (Core.NonEmpty Types.Arn)
apSigningProfileVersionArns = Lens.field @"signingProfileVersionArns"
{-# DEPRECATED apSigningProfileVersionArns "Use generic-lens or generic-optics with 'signingProfileVersionArns' instead." #-}

instance Core.FromJSON AllowedPublishers where
  toJSON AllowedPublishers {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SigningProfileVersionArns" Core..= signingProfileVersionArns)
          ]
      )

instance Core.FromJSON AllowedPublishers where
  parseJSON =
    Core.withObject "AllowedPublishers" Core.$
      \x ->
        AllowedPublishers' Core.<$> (x Core..: "SigningProfileVersionArns")
