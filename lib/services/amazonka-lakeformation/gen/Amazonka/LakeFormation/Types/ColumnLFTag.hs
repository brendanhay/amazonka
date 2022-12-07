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
-- Module      : Amazonka.LakeFormation.Types.ColumnLFTag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.ColumnLFTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.LFTagPair
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the name of a column resource and the LF-tags
-- attached to it.
--
-- /See:/ 'newColumnLFTag' smart constructor.
data ColumnLFTag = ColumnLFTag'
  { -- | The name of a column resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The LF-tags attached to a column resource.
    lFTags :: Prelude.Maybe (Prelude.NonEmpty LFTagPair)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnLFTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'columnLFTag_name' - The name of a column resource.
--
-- 'lFTags', 'columnLFTag_lFTags' - The LF-tags attached to a column resource.
newColumnLFTag ::
  ColumnLFTag
newColumnLFTag =
  ColumnLFTag'
    { name = Prelude.Nothing,
      lFTags = Prelude.Nothing
    }

-- | The name of a column resource.
columnLFTag_name :: Lens.Lens' ColumnLFTag (Prelude.Maybe Prelude.Text)
columnLFTag_name = Lens.lens (\ColumnLFTag' {name} -> name) (\s@ColumnLFTag' {} a -> s {name = a} :: ColumnLFTag)

-- | The LF-tags attached to a column resource.
columnLFTag_lFTags :: Lens.Lens' ColumnLFTag (Prelude.Maybe (Prelude.NonEmpty LFTagPair))
columnLFTag_lFTags = Lens.lens (\ColumnLFTag' {lFTags} -> lFTags) (\s@ColumnLFTag' {} a -> s {lFTags = a} :: ColumnLFTag) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ColumnLFTag where
  parseJSON =
    Data.withObject
      "ColumnLFTag"
      ( \x ->
          ColumnLFTag'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "LFTags")
      )

instance Prelude.Hashable ColumnLFTag where
  hashWithSalt _salt ColumnLFTag' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lFTags

instance Prelude.NFData ColumnLFTag where
  rnf ColumnLFTag' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf lFTags
