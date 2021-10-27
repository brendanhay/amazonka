{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Chime.Types.SigninDelegateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.SigninDelegateGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An Active Directory (AD) group whose members are granted permission to
-- act as delegates.
--
-- /See:/ 'newSigninDelegateGroup' smart constructor.
data SigninDelegateGroup = SigninDelegateGroup'
  { -- | The group name.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SigninDelegateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'signinDelegateGroup_groupName' - The group name.
newSigninDelegateGroup ::
  SigninDelegateGroup
newSigninDelegateGroup =
  SigninDelegateGroup' {groupName = Prelude.Nothing}

-- | The group name.
signinDelegateGroup_groupName :: Lens.Lens' SigninDelegateGroup (Prelude.Maybe Prelude.Text)
signinDelegateGroup_groupName = Lens.lens (\SigninDelegateGroup' {groupName} -> groupName) (\s@SigninDelegateGroup' {} a -> s {groupName = a} :: SigninDelegateGroup)

instance Core.FromJSON SigninDelegateGroup where
  parseJSON =
    Core.withObject
      "SigninDelegateGroup"
      ( \x ->
          SigninDelegateGroup'
            Prelude.<$> (x Core..:? "GroupName")
      )

instance Prelude.Hashable SigninDelegateGroup

instance Prelude.NFData SigninDelegateGroup

instance Core.ToJSON SigninDelegateGroup where
  toJSON SigninDelegateGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [("GroupName" Core..=) Prelude.<$> groupName]
      )
