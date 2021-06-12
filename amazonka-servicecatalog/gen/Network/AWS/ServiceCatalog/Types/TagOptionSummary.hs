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
-- Module      : Network.AWS.ServiceCatalog.Types.TagOptionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.TagOptionSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary information about a TagOption.
--
-- /See:/ 'newTagOptionSummary' smart constructor.
data TagOptionSummary = TagOptionSummary'
  { -- | The TagOption key.
    key :: Core.Maybe Core.Text,
    -- | The TagOption value.
    values :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagOptionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tagOptionSummary_key' - The TagOption key.
--
-- 'values', 'tagOptionSummary_values' - The TagOption value.
newTagOptionSummary ::
  TagOptionSummary
newTagOptionSummary =
  TagOptionSummary'
    { key = Core.Nothing,
      values = Core.Nothing
    }

-- | The TagOption key.
tagOptionSummary_key :: Lens.Lens' TagOptionSummary (Core.Maybe Core.Text)
tagOptionSummary_key = Lens.lens (\TagOptionSummary' {key} -> key) (\s@TagOptionSummary' {} a -> s {key = a} :: TagOptionSummary)

-- | The TagOption value.
tagOptionSummary_values :: Lens.Lens' TagOptionSummary (Core.Maybe [Core.Text])
tagOptionSummary_values = Lens.lens (\TagOptionSummary' {values} -> values) (\s@TagOptionSummary' {} a -> s {values = a} :: TagOptionSummary) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON TagOptionSummary where
  parseJSON =
    Core.withObject
      "TagOptionSummary"
      ( \x ->
          TagOptionSummary'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Values" Core..!= Core.mempty)
      )

instance Core.Hashable TagOptionSummary

instance Core.NFData TagOptionSummary
