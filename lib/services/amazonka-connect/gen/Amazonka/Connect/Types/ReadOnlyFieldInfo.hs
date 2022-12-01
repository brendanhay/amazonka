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
-- Module      : Amazonka.Connect.Types.ReadOnlyFieldInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ReadOnlyFieldInfo where

import Amazonka.Connect.Types.TaskTemplateFieldIdentifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Indicates a field that is read-only to an agent.
--
-- /See:/ 'newReadOnlyFieldInfo' smart constructor.
data ReadOnlyFieldInfo = ReadOnlyFieldInfo'
  { -- | Identifier of the read-only field.
    id :: Prelude.Maybe TaskTemplateFieldIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadOnlyFieldInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'readOnlyFieldInfo_id' - Identifier of the read-only field.
newReadOnlyFieldInfo ::
  ReadOnlyFieldInfo
newReadOnlyFieldInfo =
  ReadOnlyFieldInfo' {id = Prelude.Nothing}

-- | Identifier of the read-only field.
readOnlyFieldInfo_id :: Lens.Lens' ReadOnlyFieldInfo (Prelude.Maybe TaskTemplateFieldIdentifier)
readOnlyFieldInfo_id = Lens.lens (\ReadOnlyFieldInfo' {id} -> id) (\s@ReadOnlyFieldInfo' {} a -> s {id = a} :: ReadOnlyFieldInfo)

instance Core.FromJSON ReadOnlyFieldInfo where
  parseJSON =
    Core.withObject
      "ReadOnlyFieldInfo"
      ( \x ->
          ReadOnlyFieldInfo' Prelude.<$> (x Core..:? "Id")
      )

instance Prelude.Hashable ReadOnlyFieldInfo where
  hashWithSalt _salt ReadOnlyFieldInfo' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData ReadOnlyFieldInfo where
  rnf ReadOnlyFieldInfo' {..} = Prelude.rnf id

instance Core.ToJSON ReadOnlyFieldInfo where
  toJSON ReadOnlyFieldInfo' {..} =
    Core.object
      (Prelude.catMaybes [("Id" Core..=) Prelude.<$> id])
