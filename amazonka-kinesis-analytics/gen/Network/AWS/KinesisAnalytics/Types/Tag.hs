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
-- Module      : Network.AWS.KinesisAnalytics.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.Tag where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A key-value pair (the value is optional) that you can define and assign
-- to AWS resources. If you specify a tag that already exists, the tag
-- value is replaced with the value that you specify in the request. Note
-- that the maximum number of application tags includes system tags. The
-- maximum number of user-defined application tags is 50. For more
-- information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-tagging.html Using Tagging>.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The value of the key-value tag. The value is optional.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key of the key-value tag.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'tag_value' - The value of the key-value tag. The value is optional.
--
-- 'key', 'tag_key' - The key of the key-value tag.
newTag ::
  -- | 'key'
  Prelude.Text ->
  Tag
newTag pKey_ =
  Tag' {value = Prelude.Nothing, key = pKey_}

-- | The value of the key-value tag. The value is optional.
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | The key of the key-value tag.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

instance Prelude.FromJSON Tag where
  parseJSON =
    Prelude.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Prelude..:? "Value")
            Prelude.<*> (x Prelude..: "Key")
      )

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Prelude.ToJSON Tag where
  toJSON Tag' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Value" Prelude..=) Prelude.<$> value,
            Prelude.Just ("Key" Prelude..= key)
          ]
      )
