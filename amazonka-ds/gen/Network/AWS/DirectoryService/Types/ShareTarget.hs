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
-- Module      : Network.AWS.DirectoryService.Types.ShareTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ShareTarget where

import Network.AWS.DirectoryService.Types.TargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifier that contains details about the directory consumer account.
--
-- /See:/ 'newShareTarget' smart constructor.
data ShareTarget = ShareTarget'
  { -- | Identifier of the directory consumer account.
    id :: Prelude.Text,
    -- | Type of identifier to be used in the @Id@ field.
    type' :: TargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ShareTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'shareTarget_id' - Identifier of the directory consumer account.
--
-- 'type'', 'shareTarget_type' - Type of identifier to be used in the @Id@ field.
newShareTarget ::
  -- | 'id'
  Prelude.Text ->
  -- | 'type''
  TargetType ->
  ShareTarget
newShareTarget pId_ pType_ =
  ShareTarget' {id = pId_, type' = pType_}

-- | Identifier of the directory consumer account.
shareTarget_id :: Lens.Lens' ShareTarget Prelude.Text
shareTarget_id = Lens.lens (\ShareTarget' {id} -> id) (\s@ShareTarget' {} a -> s {id = a} :: ShareTarget)

-- | Type of identifier to be used in the @Id@ field.
shareTarget_type :: Lens.Lens' ShareTarget TargetType
shareTarget_type = Lens.lens (\ShareTarget' {type'} -> type') (\s@ShareTarget' {} a -> s {type' = a} :: ShareTarget)

instance Prelude.Hashable ShareTarget

instance Prelude.NFData ShareTarget

instance Prelude.ToJSON ShareTarget where
  toJSON ShareTarget' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Prelude..= id),
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
