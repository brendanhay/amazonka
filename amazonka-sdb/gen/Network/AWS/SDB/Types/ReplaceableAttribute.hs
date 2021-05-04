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
-- Module      : Network.AWS.SDB.Types.ReplaceableAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.ReplaceableAttribute where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable ReplaceableAttribute

instance Prelude.NFData ReplaceableAttribute

instance Prelude.ToQuery ReplaceableAttribute where
  toQuery ReplaceableAttribute' {..} =
    Prelude.mconcat
      [ "Replace" Prelude.=: replace,
        "Name" Prelude.=: name,
        "Value" Prelude.=: value
      ]
