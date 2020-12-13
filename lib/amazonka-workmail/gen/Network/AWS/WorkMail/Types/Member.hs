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
    mState,
    mDisabledDate,
    mName,
    mId,
    mType,
    mEnabledDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.MemberType

-- | The representation of a user or group.
--
-- /See:/ 'mkMember' smart constructor.
data Member = Member'
  { -- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
    state :: Lude.Maybe EntityState,
    -- | The date indicating when the member was disabled from Amazon WorkMail use.
    disabledDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the member.
    name :: Lude.Maybe Lude.Text,
    -- | The identifier of the member.
    id :: Lude.Maybe Lude.Text,
    -- | A member can be a user or group.
    type' :: Lude.Maybe MemberType,
    -- | The date indicating when the member was enabled for Amazon WorkMail use.
    enabledDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Member' with the minimum fields required to make a request.
--
-- * 'state' - The state of the member, which can be ENABLED, DISABLED, or DELETED.
-- * 'disabledDate' - The date indicating when the member was disabled from Amazon WorkMail use.
-- * 'name' - The name of the member.
-- * 'id' - The identifier of the member.
-- * 'type'' - A member can be a user or group.
-- * 'enabledDate' - The date indicating when the member was enabled for Amazon WorkMail use.
mkMember ::
  Member
mkMember =
  Member'
    { state = Lude.Nothing,
      disabledDate = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      enabledDate = Lude.Nothing
    }

-- | The state of the member, which can be ENABLED, DISABLED, or DELETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mState :: Lens.Lens' Member (Lude.Maybe EntityState)
mState = Lens.lens (state :: Member -> Lude.Maybe EntityState) (\s a -> s {state = a} :: Member)
{-# DEPRECATED mState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date indicating when the member was disabled from Amazon WorkMail use.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDisabledDate :: Lens.Lens' Member (Lude.Maybe Lude.Timestamp)
mDisabledDate = Lens.lens (disabledDate :: Member -> Lude.Maybe Lude.Timestamp) (\s a -> s {disabledDate = a} :: Member)
{-# DEPRECATED mDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The name of the member.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mName :: Lens.Lens' Member (Lude.Maybe Lude.Text)
mName = Lens.lens (name :: Member -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Member)
{-# DEPRECATED mName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the member.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mId :: Lens.Lens' Member (Lude.Maybe Lude.Text)
mId = Lens.lens (id :: Member -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Member)
{-# DEPRECATED mId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A member can be a user or group.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mType :: Lens.Lens' Member (Lude.Maybe MemberType)
mType = Lens.lens (type' :: Member -> Lude.Maybe MemberType) (\s a -> s {type' = a} :: Member)
{-# DEPRECATED mType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date indicating when the member was enabled for Amazon WorkMail use.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEnabledDate :: Lens.Lens' Member (Lude.Maybe Lude.Timestamp)
mEnabledDate = Lens.lens (enabledDate :: Member -> Lude.Maybe Lude.Timestamp) (\s a -> s {enabledDate = a} :: Member)
{-# DEPRECATED mEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

instance Lude.FromJSON Member where
  parseJSON =
    Lude.withObject
      "Member"
      ( \x ->
          Member'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "DisabledDate")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "EnabledDate")
      )
