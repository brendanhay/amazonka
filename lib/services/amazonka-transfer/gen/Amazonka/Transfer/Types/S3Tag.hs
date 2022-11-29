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
-- Module      : Amazonka.Transfer.Types.S3Tag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.S3Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the key-value pair that are assigned to a file during the
-- execution of a Tagging step.
--
-- /See:/ 'newS3Tag' smart constructor.
data S3Tag = S3Tag'
  { -- | The name assigned to the tag that you create.
    key :: Prelude.Text,
    -- | The value that corresponds to the key.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 's3Tag_key' - The name assigned to the tag that you create.
--
-- 'value', 's3Tag_value' - The value that corresponds to the key.
newS3Tag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  S3Tag
newS3Tag pKey_ pValue_ =
  S3Tag' {key = pKey_, value = pValue_}

-- | The name assigned to the tag that you create.
s3Tag_key :: Lens.Lens' S3Tag Prelude.Text
s3Tag_key = Lens.lens (\S3Tag' {key} -> key) (\s@S3Tag' {} a -> s {key = a} :: S3Tag)

-- | The value that corresponds to the key.
s3Tag_value :: Lens.Lens' S3Tag Prelude.Text
s3Tag_value = Lens.lens (\S3Tag' {value} -> value) (\s@S3Tag' {} a -> s {value = a} :: S3Tag)

instance Core.FromJSON S3Tag where
  parseJSON =
    Core.withObject
      "S3Tag"
      ( \x ->
          S3Tag'
            Prelude.<$> (x Core..: "Key") Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable S3Tag where
  hashWithSalt _salt S3Tag' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData S3Tag where
  rnf S3Tag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Core.ToJSON S3Tag where
  toJSON S3Tag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Value" Core..= value)
          ]
      )
