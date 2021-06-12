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
-- Module      : Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A regular expression representing a restriction on a string
-- configuration option value.
--
-- /See:/ 'newOptionRestrictionRegex' smart constructor.
data OptionRestrictionRegex = OptionRestrictionRegex'
  { -- | A unique name representing this regular expression.
    label :: Core.Maybe Core.Text,
    -- | The regular expression pattern that a string configuration option value
    -- with this restriction must match.
    pattern' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { label = Core.Nothing,
      pattern' = Core.Nothing
    }

-- | A unique name representing this regular expression.
optionRestrictionRegex_label :: Lens.Lens' OptionRestrictionRegex (Core.Maybe Core.Text)
optionRestrictionRegex_label = Lens.lens (\OptionRestrictionRegex' {label} -> label) (\s@OptionRestrictionRegex' {} a -> s {label = a} :: OptionRestrictionRegex)

-- | The regular expression pattern that a string configuration option value
-- with this restriction must match.
optionRestrictionRegex_pattern :: Lens.Lens' OptionRestrictionRegex (Core.Maybe Core.Text)
optionRestrictionRegex_pattern = Lens.lens (\OptionRestrictionRegex' {pattern'} -> pattern') (\s@OptionRestrictionRegex' {} a -> s {pattern' = a} :: OptionRestrictionRegex)

instance Core.FromXML OptionRestrictionRegex where
  parseXML x =
    OptionRestrictionRegex'
      Core.<$> (x Core..@? "Label") Core.<*> (x Core..@? "Pattern")

instance Core.Hashable OptionRestrictionRegex

instance Core.NFData OptionRestrictionRegex
