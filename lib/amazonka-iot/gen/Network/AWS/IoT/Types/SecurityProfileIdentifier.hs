{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SecurityProfileIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SecurityProfileIdentifier
  ( SecurityProfileIdentifier (..),

    -- * Smart constructor
    mkSecurityProfileIdentifier,

    -- * Lenses
    spiName,
    spiArn,
  )
where

import qualified Network.AWS.IoT.Types.SecurityProfileArn as Types
import qualified Network.AWS.IoT.Types.SecurityProfileName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifying information for a Device Defender security profile.
--
-- /See:/ 'mkSecurityProfileIdentifier' smart constructor.
data SecurityProfileIdentifier = SecurityProfileIdentifier'
  { -- | The name you have given to the security profile.
    name :: Types.SecurityProfileName,
    -- | The ARN of the security profile.
    arn :: Types.SecurityProfileArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityProfileIdentifier' value with any optional fields omitted.
mkSecurityProfileIdentifier ::
  -- | 'name'
  Types.SecurityProfileName ->
  -- | 'arn'
  Types.SecurityProfileArn ->
  SecurityProfileIdentifier
mkSecurityProfileIdentifier name arn =
  SecurityProfileIdentifier' {name, arn}

-- | The name you have given to the security profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spiName :: Lens.Lens' SecurityProfileIdentifier Types.SecurityProfileName
spiName = Lens.field @"name"
{-# DEPRECATED spiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the security profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spiArn :: Lens.Lens' SecurityProfileIdentifier Types.SecurityProfileArn
spiArn = Lens.field @"arn"
{-# DEPRECATED spiArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Core.FromJSON SecurityProfileIdentifier where
  parseJSON =
    Core.withObject "SecurityProfileIdentifier" Core.$
      \x ->
        SecurityProfileIdentifier'
          Core.<$> (x Core..: "name") Core.<*> (x Core..: "arn")
