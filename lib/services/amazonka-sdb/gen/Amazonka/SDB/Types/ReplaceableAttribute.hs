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
-- Module      : Amazonka.SDB.Types.ReplaceableAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SDB.Types.ReplaceableAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newReplaceableAttribute' smart constructor.
data ReplaceableAttribute = ReplaceableAttribute'
  { -- | A flag specifying whether or not to replace the attribute\/value pair or
    -- to add a new attribute\/value pair. The default setting is @false@.
    replace :: Prelude.Maybe Prelude.Bool,
    -- | The name of the replaceable attribute.
    name :: Prelude.Text,
    -- | The value of the replaceable attribute.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplaceableAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replace', 'replaceableAttribute_replace' - A flag specifying whether or not to replace the attribute\/value pair or
-- to add a new attribute\/value pair. The default setting is @false@.
--
-- 'name', 'replaceableAttribute_name' - The name of the replaceable attribute.
--
-- 'value', 'replaceableAttribute_value' - The value of the replaceable attribute.
newReplaceableAttribute ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  ReplaceableAttribute
newReplaceableAttribute pName_ pValue_ =
  ReplaceableAttribute'
    { replace = Prelude.Nothing,
      name = pName_,
      value = pValue_
    }

-- | A flag specifying whether or not to replace the attribute\/value pair or
-- to add a new attribute\/value pair. The default setting is @false@.
replaceableAttribute_replace :: Lens.Lens' ReplaceableAttribute (Prelude.Maybe Prelude.Bool)
replaceableAttribute_replace = Lens.lens (\ReplaceableAttribute' {replace} -> replace) (\s@ReplaceableAttribute' {} a -> s {replace = a} :: ReplaceableAttribute)

-- | The name of the replaceable attribute.
replaceableAttribute_name :: Lens.Lens' ReplaceableAttribute Prelude.Text
replaceableAttribute_name = Lens.lens (\ReplaceableAttribute' {name} -> name) (\s@ReplaceableAttribute' {} a -> s {name = a} :: ReplaceableAttribute)

-- | The value of the replaceable attribute.
replaceableAttribute_value :: Lens.Lens' ReplaceableAttribute Prelude.Text
replaceableAttribute_value = Lens.lens (\ReplaceableAttribute' {value} -> value) (\s@ReplaceableAttribute' {} a -> s {value = a} :: ReplaceableAttribute)

instance Prelude.Hashable ReplaceableAttribute where
  hashWithSalt _salt ReplaceableAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` replace
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData ReplaceableAttribute where
  rnf ReplaceableAttribute' {..} =
    Prelude.rnf replace
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf value

instance Data.ToQuery ReplaceableAttribute where
  toQuery ReplaceableAttribute' {..} =
    Prelude.mconcat
      [ "Replace" Data.=: replace,
        "Name" Data.=: name,
        "Value" Data.=: value
      ]
