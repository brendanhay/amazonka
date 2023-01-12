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
-- Module      : Amazonka.Connect.Types.RequiredFieldInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RequiredFieldInfo where

import Amazonka.Connect.Types.TaskTemplateFieldIdentifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a required field.
--
-- /See:/ 'newRequiredFieldInfo' smart constructor.
data RequiredFieldInfo = RequiredFieldInfo'
  { -- | The unique identifier for the field.
    id :: Prelude.Maybe TaskTemplateFieldIdentifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequiredFieldInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'requiredFieldInfo_id' - The unique identifier for the field.
newRequiredFieldInfo ::
  RequiredFieldInfo
newRequiredFieldInfo =
  RequiredFieldInfo' {id = Prelude.Nothing}

-- | The unique identifier for the field.
requiredFieldInfo_id :: Lens.Lens' RequiredFieldInfo (Prelude.Maybe TaskTemplateFieldIdentifier)
requiredFieldInfo_id = Lens.lens (\RequiredFieldInfo' {id} -> id) (\s@RequiredFieldInfo' {} a -> s {id = a} :: RequiredFieldInfo)

instance Data.FromJSON RequiredFieldInfo where
  parseJSON =
    Data.withObject
      "RequiredFieldInfo"
      ( \x ->
          RequiredFieldInfo' Prelude.<$> (x Data..:? "Id")
      )

instance Prelude.Hashable RequiredFieldInfo where
  hashWithSalt _salt RequiredFieldInfo' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData RequiredFieldInfo where
  rnf RequiredFieldInfo' {..} = Prelude.rnf id

instance Data.ToJSON RequiredFieldInfo where
  toJSON RequiredFieldInfo' {..} =
    Data.object
      (Prelude.catMaybes [("Id" Data..=) Prelude.<$> id])
