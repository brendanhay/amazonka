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
-- Module      : Amazonka.FinSpace.Types.SuperuserParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.SuperuserParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for the superuser.
--
-- /See:/ 'newSuperuserParameters' smart constructor.
data SuperuserParameters = SuperuserParameters'
  { -- | The email address of the superuser.
    emailAddress :: Data.Sensitive Prelude.Text,
    -- | The first name of the superuser.
    firstName :: Prelude.Text,
    -- | The last name of the superuser.
    lastName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuperuserParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailAddress', 'superuserParameters_emailAddress' - The email address of the superuser.
--
-- 'firstName', 'superuserParameters_firstName' - The first name of the superuser.
--
-- 'lastName', 'superuserParameters_lastName' - The last name of the superuser.
newSuperuserParameters ::
  -- | 'emailAddress'
  Prelude.Text ->
  -- | 'firstName'
  Prelude.Text ->
  -- | 'lastName'
  Prelude.Text ->
  SuperuserParameters
newSuperuserParameters
  pEmailAddress_
  pFirstName_
  pLastName_ =
    SuperuserParameters'
      { emailAddress =
          Data._Sensitive Lens.# pEmailAddress_,
        firstName = pFirstName_,
        lastName = pLastName_
      }

-- | The email address of the superuser.
superuserParameters_emailAddress :: Lens.Lens' SuperuserParameters Prelude.Text
superuserParameters_emailAddress = Lens.lens (\SuperuserParameters' {emailAddress} -> emailAddress) (\s@SuperuserParameters' {} a -> s {emailAddress = a} :: SuperuserParameters) Prelude.. Data._Sensitive

-- | The first name of the superuser.
superuserParameters_firstName :: Lens.Lens' SuperuserParameters Prelude.Text
superuserParameters_firstName = Lens.lens (\SuperuserParameters' {firstName} -> firstName) (\s@SuperuserParameters' {} a -> s {firstName = a} :: SuperuserParameters)

-- | The last name of the superuser.
superuserParameters_lastName :: Lens.Lens' SuperuserParameters Prelude.Text
superuserParameters_lastName = Lens.lens (\SuperuserParameters' {lastName} -> lastName) (\s@SuperuserParameters' {} a -> s {lastName = a} :: SuperuserParameters)

instance Prelude.Hashable SuperuserParameters where
  hashWithSalt _salt SuperuserParameters' {..} =
    _salt `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` lastName

instance Prelude.NFData SuperuserParameters where
  rnf SuperuserParameters' {..} =
    Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf lastName

instance Data.ToJSON SuperuserParameters where
  toJSON SuperuserParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("emailAddress" Data..= emailAddress),
            Prelude.Just ("firstName" Data..= firstName),
            Prelude.Just ("lastName" Data..= lastName)
          ]
      )
