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
-- Module      : Amazonka.Connect.Types.InvisibleFieldInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.InvisibleFieldInfo where

import Amazonka.Connect.Types.TaskTemplateFieldIdentifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A field that is invisible to an agent.
--
-- /See:/ 'newInvisibleFieldInfo' smart constructor.
data InvisibleFieldInfo = InvisibleFieldInfo'
  { -- | Identifier of the invisible field.
    id :: Prelude.Maybe TaskTemplateFieldIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvisibleFieldInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'invisibleFieldInfo_id' - Identifier of the invisible field.
newInvisibleFieldInfo ::
  InvisibleFieldInfo
newInvisibleFieldInfo =
  InvisibleFieldInfo' {id = Prelude.Nothing}

-- | Identifier of the invisible field.
invisibleFieldInfo_id :: Lens.Lens' InvisibleFieldInfo (Prelude.Maybe TaskTemplateFieldIdentifier)
invisibleFieldInfo_id = Lens.lens (\InvisibleFieldInfo' {id} -> id) (\s@InvisibleFieldInfo' {} a -> s {id = a} :: InvisibleFieldInfo)

instance Core.FromJSON InvisibleFieldInfo where
  parseJSON =
    Core.withObject
      "InvisibleFieldInfo"
      ( \x ->
          InvisibleFieldInfo' Prelude.<$> (x Core..:? "Id")
      )

instance Prelude.Hashable InvisibleFieldInfo where
  hashWithSalt _salt InvisibleFieldInfo' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData InvisibleFieldInfo where
  rnf InvisibleFieldInfo' {..} = Prelude.rnf id

instance Core.ToJSON InvisibleFieldInfo where
  toJSON InvisibleFieldInfo' {..} =
    Core.object
      (Prelude.catMaybes [("Id" Core..=) Prelude.<$> id])
