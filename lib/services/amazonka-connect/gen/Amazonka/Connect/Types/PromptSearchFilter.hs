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
-- Module      : Amazonka.Connect.Types.PromptSearchFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.PromptSearchFilter where

import Amazonka.Connect.Types.ControlPlaneTagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters to be applied to search results.
--
-- /See:/ 'newPromptSearchFilter' smart constructor.
data PromptSearchFilter = PromptSearchFilter'
  { tagFilter :: Prelude.Maybe ControlPlaneTagFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromptSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilter', 'promptSearchFilter_tagFilter' - Undocumented member.
newPromptSearchFilter ::
  PromptSearchFilter
newPromptSearchFilter =
  PromptSearchFilter' {tagFilter = Prelude.Nothing}

-- | Undocumented member.
promptSearchFilter_tagFilter :: Lens.Lens' PromptSearchFilter (Prelude.Maybe ControlPlaneTagFilter)
promptSearchFilter_tagFilter = Lens.lens (\PromptSearchFilter' {tagFilter} -> tagFilter) (\s@PromptSearchFilter' {} a -> s {tagFilter = a} :: PromptSearchFilter)

instance Prelude.Hashable PromptSearchFilter where
  hashWithSalt _salt PromptSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` tagFilter

instance Prelude.NFData PromptSearchFilter where
  rnf PromptSearchFilter' {..} = Prelude.rnf tagFilter

instance Data.ToJSON PromptSearchFilter where
  toJSON PromptSearchFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("TagFilter" Data..=) Prelude.<$> tagFilter]
      )
