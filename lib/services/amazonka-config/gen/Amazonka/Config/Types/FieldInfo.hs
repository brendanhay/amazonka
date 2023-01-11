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
-- Module      : Amazonka.Config.Types.FieldInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.FieldInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the fields such as name of the field.
--
-- /See:/ 'newFieldInfo' smart constructor.
data FieldInfo = FieldInfo'
  { -- | Name of the field.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'fieldInfo_name' - Name of the field.
newFieldInfo ::
  FieldInfo
newFieldInfo = FieldInfo' {name = Prelude.Nothing}

-- | Name of the field.
fieldInfo_name :: Lens.Lens' FieldInfo (Prelude.Maybe Prelude.Text)
fieldInfo_name = Lens.lens (\FieldInfo' {name} -> name) (\s@FieldInfo' {} a -> s {name = a} :: FieldInfo)

instance Data.FromJSON FieldInfo where
  parseJSON =
    Data.withObject
      "FieldInfo"
      (\x -> FieldInfo' Prelude.<$> (x Data..:? "Name"))

instance Prelude.Hashable FieldInfo where
  hashWithSalt _salt FieldInfo' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData FieldInfo where
  rnf FieldInfo' {..} = Prelude.rnf name
