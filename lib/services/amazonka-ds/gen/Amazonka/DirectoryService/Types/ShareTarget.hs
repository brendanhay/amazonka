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
-- Module      : Amazonka.DirectoryService.Types.ShareTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.ShareTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.TargetType
import qualified Amazonka.Prelude as Prelude

-- | Identifier that contains details about the directory consumer account.
--
-- /See:/ 'newShareTarget' smart constructor.
data ShareTarget = ShareTarget'
  { -- | Identifier of the directory consumer account.
    id :: Prelude.Text,
    -- | Type of identifier to be used in the @Id@ field.
    type' :: TargetType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.Hashable ShareTarget where
  hashWithSalt _salt ShareTarget' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ShareTarget where
  rnf ShareTarget' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON ShareTarget where
  toJSON ShareTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Type" Data..= type')
          ]
      )
