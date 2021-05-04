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
-- Module      : Network.AWS.DirectoryService.Types.UnshareTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.UnshareTarget where

import Network.AWS.DirectoryService.Types.TargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifier that contains details about the directory consumer account
-- with whom the directory is being unshared.
--
-- /See:/ 'newUnshareTarget' smart constructor.
data UnshareTarget = UnshareTarget'
  { -- | Identifier of the directory consumer account.
    id :: Prelude.Text,
    -- | Type of identifier to be used in the /Id/ field.
    type' :: TargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnshareTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'unshareTarget_id' - Identifier of the directory consumer account.
--
-- 'type'', 'unshareTarget_type' - Type of identifier to be used in the /Id/ field.
newUnshareTarget ::
  -- | 'id'
  Prelude.Text ->
  -- | 'type''
  TargetType ->
  UnshareTarget
newUnshareTarget pId_ pType_ =
  UnshareTarget' {id = pId_, type' = pType_}

-- | Identifier of the directory consumer account.
unshareTarget_id :: Lens.Lens' UnshareTarget Prelude.Text
unshareTarget_id = Lens.lens (\UnshareTarget' {id} -> id) (\s@UnshareTarget' {} a -> s {id = a} :: UnshareTarget)

-- | Type of identifier to be used in the /Id/ field.
unshareTarget_type :: Lens.Lens' UnshareTarget TargetType
unshareTarget_type = Lens.lens (\UnshareTarget' {type'} -> type') (\s@UnshareTarget' {} a -> s {type' = a} :: UnshareTarget)

instance Prelude.Hashable UnshareTarget

instance Prelude.NFData UnshareTarget

instance Prelude.ToJSON UnshareTarget where
  toJSON UnshareTarget' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Prelude..= id),
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
