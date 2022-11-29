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
-- Module      : Amazonka.IAM.Types.Tag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that represents user-provided metadata that can be
-- associated with an IAM resource. For more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key name that can be used to look up or retrieve the associated
    -- value. For example, @Department@ or @Cost Center@ are common choices.
    key :: Prelude.Text,
    -- | The value associated with this tag. For example, tags with a key name of
    -- @Department@ could have values such as @Human Resources@, @Accounting@,
    -- and @Support@. Tags with a key name of @Cost Center@ might have values
    -- that consist of the number associated with the different cost centers in
    -- your company. Typically, many resources have tags with the same key name
    -- but with different values.
    --
    -- Amazon Web Services always interprets the tag @Value@ as a single
    -- string. If you need to store an array, you can store comma-separated
    -- values in the string. However, you must interpret the value in your
    -- code.
    value :: Prelude.Text
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
-- 'key', 'tag_key' - The key name that can be used to look up or retrieve the associated
-- value. For example, @Department@ or @Cost Center@ are common choices.
--
-- 'value', 'tag_value' - The value associated with this tag. For example, tags with a key name of
-- @Department@ could have values such as @Human Resources@, @Accounting@,
-- and @Support@. Tags with a key name of @Cost Center@ might have values
-- that consist of the number associated with the different cost centers in
-- your company. Typically, many resources have tags with the same key name
-- but with different values.
--
-- Amazon Web Services always interprets the tag @Value@ as a single
-- string. If you need to store an array, you can store comma-separated
-- values in the string. However, you must interpret the value in your
-- code.
newTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | The key name that can be used to look up or retrieve the associated
-- value. For example, @Department@ or @Cost Center@ are common choices.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value associated with this tag. For example, tags with a key name of
-- @Department@ could have values such as @Human Resources@, @Accounting@,
-- and @Support@. Tags with a key name of @Cost Center@ might have values
-- that consist of the number associated with the different cost centers in
-- your company. Typically, many resources have tags with the same key name
-- but with different values.
--
-- Amazon Web Services always interprets the tag @Value@ as a single
-- string. If you need to store an array, you can store comma-separated
-- values in the string. However, you must interpret the value in your
-- code.
tag_value :: Lens.Lens' Tag Prelude.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromXML Tag where
  parseXML x =
    Tag'
      Prelude.<$> (x Core..@ "Key") Prelude.<*> (x Core..@ "Value")

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Core.ToQuery Tag where
  toQuery Tag' {..} =
    Prelude.mconcat
      ["Key" Core.=: key, "Value" Core.=: value]
