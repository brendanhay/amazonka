{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Member
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Member
  ( Member (..),

    -- * Smart constructor
    mkMember,

    -- * Lenses
    mDisabledDate,
    mEnabledDate,
    mId,
    mName,
    mState,
    mType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.EntityState as Types
import qualified Network.AWS.WorkMail.Types.MemberType as Types
import qualified Network.AWS.WorkMail.Types.String as Types

-- | The representation of a user or group.
--
-- /See:/ 'mkMember' smart constructor.
data Member = Member'
  { -- | The date indicating when the member was disabled from Amazon WorkMail use.
    disabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The date indicating when the member was enabled for Amazon WorkMail use.
    enabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The identifier of the member.
    id :: Core.Maybe Types.String,
    -- | The name of the member.
    name :: Core.Maybe Types.String,
    -- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
    state :: Core.Maybe Types.EntityState,
    -- | A member can be a user or group.
    type' :: Core.Maybe Types.MemberType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Member' value with any optional fields omitted.
mkMember ::
  Member
mkMember =
  Member'
    { disabledDate = Core.Nothing,
      enabledDate = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      state = Core.Nothing,
      type' = Core.Nothing
    }

-- | The date indicating when the member was disabled from Amazon WorkMail use.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDisabledDate :: Lens.Lens' Member (Core.Maybe Core.NominalDiffTime)
mDisabledDate = Lens.field @"disabledDate"
{-# DEPRECATED mDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The date indicating when the member was enabled for Amazon WorkMail use.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEnabledDate :: Lens.Lens' Member (Core.Maybe Core.NominalDiffTime)
mEnabledDate = Lens.field @"enabledDate"
{-# DEPRECATED mEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

-- | The identifier of the member.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mId :: Lens.Lens' Member (Core.Maybe Types.String)
mId = Lens.field @"id"
{-# DEPRECATED mId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the member.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mName :: Lens.Lens' Member (Core.Maybe Types.String)
mName = Lens.field @"name"
{-# DEPRECATED mName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mState :: Lens.Lens' Member (Core.Maybe Types.EntityState)
mState = Lens.field @"state"
{-# DEPRECATED mState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A member can be a user or group.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mType :: Lens.Lens' Member (Core.Maybe Types.MemberType)
mType = Lens.field @"type'"
{-# DEPRECATED mType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Member where
  parseJSON =
    Core.withObject "Member" Core.$
      \x ->
        Member'
          Core.<$> (x Core..:? "DisabledDate")
          Core.<*> (x Core..:? "EnabledDate")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "Type")
