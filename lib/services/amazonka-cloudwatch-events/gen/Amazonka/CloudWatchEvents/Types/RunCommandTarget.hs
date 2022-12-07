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
-- Module      : Amazonka.CloudWatchEvents.Types.RunCommandTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.RunCommandTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the EC2 instances that are to be sent the command,
-- specified as key-value pairs. Each @RunCommandTarget@ block can include
-- only one key, but this key may specify multiple values.
--
-- /See:/ 'newRunCommandTarget' smart constructor.
data RunCommandTarget = RunCommandTarget'
  { -- | Can be either @tag:@ /tag-key/ or @InstanceIds@.
    key :: Prelude.Text,
    -- | If @Key@ is @tag:@ /tag-key/, @Values@ is a list of tag values. If @Key@
    -- is @InstanceIds@, @Values@ is a list of Amazon EC2 instance IDs.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RunCommandTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'runCommandTarget_key' - Can be either @tag:@ /tag-key/ or @InstanceIds@.
--
-- 'values', 'runCommandTarget_values' - If @Key@ is @tag:@ /tag-key/, @Values@ is a list of tag values. If @Key@
-- is @InstanceIds@, @Values@ is a list of Amazon EC2 instance IDs.
newRunCommandTarget ::
  -- | 'key'
  Prelude.Text ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  RunCommandTarget
newRunCommandTarget pKey_ pValues_ =
  RunCommandTarget'
    { key = pKey_,
      values = Lens.coerced Lens.# pValues_
    }

-- | Can be either @tag:@ /tag-key/ or @InstanceIds@.
runCommandTarget_key :: Lens.Lens' RunCommandTarget Prelude.Text
runCommandTarget_key = Lens.lens (\RunCommandTarget' {key} -> key) (\s@RunCommandTarget' {} a -> s {key = a} :: RunCommandTarget)

-- | If @Key@ is @tag:@ /tag-key/, @Values@ is a list of tag values. If @Key@
-- is @InstanceIds@, @Values@ is a list of Amazon EC2 instance IDs.
runCommandTarget_values :: Lens.Lens' RunCommandTarget (Prelude.NonEmpty Prelude.Text)
runCommandTarget_values = Lens.lens (\RunCommandTarget' {values} -> values) (\s@RunCommandTarget' {} a -> s {values = a} :: RunCommandTarget) Prelude.. Lens.coerced

instance Data.FromJSON RunCommandTarget where
  parseJSON =
    Data.withObject
      "RunCommandTarget"
      ( \x ->
          RunCommandTarget'
            Prelude.<$> (x Data..: "Key") Prelude.<*> (x Data..: "Values")
      )

instance Prelude.Hashable RunCommandTarget where
  hashWithSalt _salt RunCommandTarget' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData RunCommandTarget where
  rnf RunCommandTarget' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Data.ToJSON RunCommandTarget where
  toJSON RunCommandTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values)
          ]
      )
