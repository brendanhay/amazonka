{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSecurityGroup
  ( InputSecurityGroup (..),

    -- * Smart constructor
    mkInputSecurityGroup,

    -- * Lenses
    isgArn,
    isgId,
    isgInputs,
    isgState,
    isgTags,
    isgWhitelistRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputSecurityGroupState as Types
import qualified Network.AWS.MediaLive.Types.InputWhitelistRule as Types
import qualified Network.AWS.Prelude as Core

-- | An Input Security Group
--
-- /See:/ 'mkInputSecurityGroup' smart constructor.
data InputSecurityGroup = InputSecurityGroup'
  { -- | Unique ARN of Input Security Group
    arn :: Core.Maybe Core.Text,
    -- | The Id of the Input Security Group
    id :: Core.Maybe Core.Text,
    -- | The list of inputs currently using this Input Security Group.
    inputs :: Core.Maybe [Core.Text],
    -- | The current state of the Input Security Group.
    state :: Core.Maybe Types.InputSecurityGroupState,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Whitelist rules and their sync status
    whitelistRules :: Core.Maybe [Types.InputWhitelistRule]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputSecurityGroup' value with any optional fields omitted.
mkInputSecurityGroup ::
  InputSecurityGroup
mkInputSecurityGroup =
  InputSecurityGroup'
    { arn = Core.Nothing,
      id = Core.Nothing,
      inputs = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      whitelistRules = Core.Nothing
    }

-- | Unique ARN of Input Security Group
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgArn :: Lens.Lens' InputSecurityGroup (Core.Maybe Core.Text)
isgArn = Lens.field @"arn"
{-# DEPRECATED isgArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The Id of the Input Security Group
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgId :: Lens.Lens' InputSecurityGroup (Core.Maybe Core.Text)
isgId = Lens.field @"id"
{-# DEPRECATED isgId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The list of inputs currently using this Input Security Group.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgInputs :: Lens.Lens' InputSecurityGroup (Core.Maybe [Core.Text])
isgInputs = Lens.field @"inputs"
{-# DEPRECATED isgInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | The current state of the Input Security Group.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgState :: Lens.Lens' InputSecurityGroup (Core.Maybe Types.InputSecurityGroupState)
isgState = Lens.field @"state"
{-# DEPRECATED isgState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A collection of key-value pairs.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgTags :: Lens.Lens' InputSecurityGroup (Core.Maybe (Core.HashMap Core.Text Core.Text))
isgTags = Lens.field @"tags"
{-# DEPRECATED isgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Whitelist rules and their sync status
--
-- /Note:/ Consider using 'whitelistRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isgWhitelistRules :: Lens.Lens' InputSecurityGroup (Core.Maybe [Types.InputWhitelistRule])
isgWhitelistRules = Lens.field @"whitelistRules"
{-# DEPRECATED isgWhitelistRules "Use generic-lens or generic-optics with 'whitelistRules' instead." #-}

instance Core.FromJSON InputSecurityGroup where
  parseJSON =
    Core.withObject "InputSecurityGroup" Core.$
      \x ->
        InputSecurityGroup'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "inputs")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "whitelistRules")
