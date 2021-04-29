{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkMail.Types.Delegate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Delegate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkMail.Types.MemberType

-- | The name of the attribute, which is one of the values defined in the
-- UserAttribute enumeration.
--
-- /See:/ 'newDelegate' smart constructor.
data Delegate = Delegate'
  { -- | The identifier for the user or group associated as the resource\'s
    -- delegate.
    id :: Prelude.Text,
    -- | The type of the delegate: user or group.
    type' :: MemberType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Delegate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'delegate_id' - The identifier for the user or group associated as the resource\'s
-- delegate.
--
-- 'type'', 'delegate_type' - The type of the delegate: user or group.
newDelegate ::
  -- | 'id'
  Prelude.Text ->
  -- | 'type''
  MemberType ->
  Delegate
newDelegate pId_ pType_ =
  Delegate' {id = pId_, type' = pType_}

-- | The identifier for the user or group associated as the resource\'s
-- delegate.
delegate_id :: Lens.Lens' Delegate Prelude.Text
delegate_id = Lens.lens (\Delegate' {id} -> id) (\s@Delegate' {} a -> s {id = a} :: Delegate)

-- | The type of the delegate: user or group.
delegate_type :: Lens.Lens' Delegate MemberType
delegate_type = Lens.lens (\Delegate' {type'} -> type') (\s@Delegate' {} a -> s {type' = a} :: Delegate)

instance Prelude.FromJSON Delegate where
  parseJSON =
    Prelude.withObject
      "Delegate"
      ( \x ->
          Delegate'
            Prelude.<$> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "Type")
      )

instance Prelude.Hashable Delegate

instance Prelude.NFData Delegate
