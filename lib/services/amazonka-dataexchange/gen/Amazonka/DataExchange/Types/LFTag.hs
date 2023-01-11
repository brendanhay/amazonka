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
-- Module      : Amazonka.DataExchange.Types.LFTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.LFTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that allows an LF-admin to grant permissions on certain
-- conditions.
--
-- /See:/ 'newLFTag' smart constructor.
data LFTag = LFTag'
  { -- | The key name for the LF-tag.
    tagKey :: Prelude.Text,
    -- | A list of LF-tag values.
    tagValues :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LFTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKey', 'lFTag_tagKey' - The key name for the LF-tag.
--
-- 'tagValues', 'lFTag_tagValues' - A list of LF-tag values.
newLFTag ::
  -- | 'tagKey'
  Prelude.Text ->
  LFTag
newLFTag pTagKey_ =
  LFTag'
    { tagKey = pTagKey_,
      tagValues = Prelude.mempty
    }

-- | The key name for the LF-tag.
lFTag_tagKey :: Lens.Lens' LFTag Prelude.Text
lFTag_tagKey = Lens.lens (\LFTag' {tagKey} -> tagKey) (\s@LFTag' {} a -> s {tagKey = a} :: LFTag)

-- | A list of LF-tag values.
lFTag_tagValues :: Lens.Lens' LFTag [Prelude.Text]
lFTag_tagValues = Lens.lens (\LFTag' {tagValues} -> tagValues) (\s@LFTag' {} a -> s {tagValues = a} :: LFTag) Prelude.. Lens.coerced

instance Data.FromJSON LFTag where
  parseJSON =
    Data.withObject
      "LFTag"
      ( \x ->
          LFTag'
            Prelude.<$> (x Data..: "TagKey")
            Prelude.<*> (x Data..:? "TagValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LFTag where
  hashWithSalt _salt LFTag' {..} =
    _salt `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData LFTag where
  rnf LFTag' {..} =
    Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValues

instance Data.ToJSON LFTag where
  toJSON LFTag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TagKey" Data..= tagKey),
            Prelude.Just ("TagValues" Data..= tagValues)
          ]
      )
