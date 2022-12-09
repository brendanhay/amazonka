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
-- Module      : Amazonka.ElasticBeanstalk.Types.OptionRestrictionRegex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.OptionRestrictionRegex where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A regular expression representing a restriction on a string
-- configuration option value.
--
-- /See:/ 'newOptionRestrictionRegex' smart constructor.
data OptionRestrictionRegex = OptionRestrictionRegex'
  { -- | A unique name representing this regular expression.
    label :: Prelude.Maybe Prelude.Text,
    -- | The regular expression pattern that a string configuration option value
    -- with this restriction must match.
    pattern' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptionRestrictionRegex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'optionRestrictionRegex_label' - A unique name representing this regular expression.
--
-- 'pattern'', 'optionRestrictionRegex_pattern' - The regular expression pattern that a string configuration option value
-- with this restriction must match.
newOptionRestrictionRegex ::
  OptionRestrictionRegex
newOptionRestrictionRegex =
  OptionRestrictionRegex'
    { label = Prelude.Nothing,
      pattern' = Prelude.Nothing
    }

-- | A unique name representing this regular expression.
optionRestrictionRegex_label :: Lens.Lens' OptionRestrictionRegex (Prelude.Maybe Prelude.Text)
optionRestrictionRegex_label = Lens.lens (\OptionRestrictionRegex' {label} -> label) (\s@OptionRestrictionRegex' {} a -> s {label = a} :: OptionRestrictionRegex)

-- | The regular expression pattern that a string configuration option value
-- with this restriction must match.
optionRestrictionRegex_pattern :: Lens.Lens' OptionRestrictionRegex (Prelude.Maybe Prelude.Text)
optionRestrictionRegex_pattern = Lens.lens (\OptionRestrictionRegex' {pattern'} -> pattern') (\s@OptionRestrictionRegex' {} a -> s {pattern' = a} :: OptionRestrictionRegex)

instance Data.FromXML OptionRestrictionRegex where
  parseXML x =
    OptionRestrictionRegex'
      Prelude.<$> (x Data..@? "Label")
      Prelude.<*> (x Data..@? "Pattern")

instance Prelude.Hashable OptionRestrictionRegex where
  hashWithSalt _salt OptionRestrictionRegex' {..} =
    _salt `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` pattern'

instance Prelude.NFData OptionRestrictionRegex where
  rnf OptionRestrictionRegex' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf pattern'
