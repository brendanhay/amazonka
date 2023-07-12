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
-- Module      : Amazonka.Chime.Types.SigninDelegateGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.SigninDelegateGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON SigninDelegateGroup where
  parseJSON =
    Data.withObject
      "SigninDelegateGroup"
      ( \x ->
          SigninDelegateGroup'
            Prelude.<$> (x Data..:? "GroupName")
      )

instance Prelude.Hashable SigninDelegateGroup where
  hashWithSalt _salt SigninDelegateGroup' {..} =
    _salt `Prelude.hashWithSalt` groupName

instance Prelude.NFData SigninDelegateGroup where
  rnf SigninDelegateGroup' {..} = Prelude.rnf groupName

instance Data.ToJSON SigninDelegateGroup where
  toJSON SigninDelegateGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [("GroupName" Data..=) Prelude.<$> groupName]
      )
