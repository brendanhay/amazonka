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
-- Module      : Amazonka.KMS.Types.Tag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair. A tag consists of a tag key and a tag value. Tag keys
-- and tag values are both required, but tag values can be empty (null)
-- strings.
--
-- For information about the rules that apply to tag keys and tag values,
-- see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions>
-- in the /Amazon Web Services Billing and Cost Management User Guide/.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key of the tag.
    tagKey :: Prelude.Text,
    -- | The value of the tag.
    tagValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKey', 'tag_tagKey' - The key of the tag.
--
-- 'tagValue', 'tag_tagValue' - The value of the tag.
newTag ::
  -- | 'tagKey'
  Prelude.Text ->
  -- | 'tagValue'
  Prelude.Text ->
  Tag
newTag pTagKey_ pTagValue_ =
  Tag' {tagKey = pTagKey_, tagValue = pTagValue_}

-- | The key of the tag.
tag_tagKey :: Lens.Lens' Tag Prelude.Text
tag_tagKey = Lens.lens (\Tag' {tagKey} -> tagKey) (\s@Tag' {} a -> s {tagKey = a} :: Tag)

-- | The value of the tag.
tag_tagValue :: Lens.Lens' Tag Prelude.Text
tag_tagValue = Lens.lens (\Tag' {tagValue} -> tagValue) (\s@Tag' {} a -> s {tagValue = a} :: Tag)

instance Data.FromJSON Tag where
  parseJSON =
    Data.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Data..: "TagKey")
            Prelude.<*> (x Data..: "TagValue")
      )

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt
      `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` tagValue

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValue

instance Data.ToJSON Tag where
  toJSON Tag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TagKey" Data..= tagKey),
            Prelude.Just ("TagValue" Data..= tagValue)
          ]
      )
