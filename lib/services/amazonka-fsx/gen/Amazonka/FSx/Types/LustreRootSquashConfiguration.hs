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
-- Module      : Amazonka.FSx.Types.LustreRootSquashConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.LustreRootSquashConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for Lustre root squash used to restrict root-level
-- access from clients that try to access your FSx for Lustre file system
-- as root. Use the @RootSquash@ parameter to enable root squash. To learn
-- more about Lustre root squash, see
-- <https://docs.aws.amazon.com/fsx/latest/LustreGuide/root-squash.html Lustre root squash>.
--
-- You can also use the @NoSquashNids@ parameter to provide an array of
-- clients who are not affected by the root squash setting. These clients
-- will access the file system as root, with unrestricted privileges.
--
-- /See:/ 'newLustreRootSquashConfiguration' smart constructor.
data LustreRootSquashConfiguration = LustreRootSquashConfiguration'
  { -- | When root squash is enabled, you can optionally specify an array of NIDs
    -- of clients for which root squash does not apply. A client NID is a
    -- Lustre Network Identifier used to uniquely identify a client. You can
    -- specify the NID as either a single address or a range of addresses:
    --
    -- -   A single address is described in standard Lustre NID format by
    --     specifying the client’s IP address followed by the Lustre network ID
    --     (for example, @10.0.1.6\@tcp@).
    --
    -- -   An address range is described using a dash to separate the range
    --     (for example, @10.0.[2-10].[1-255]\@tcp@).
    noSquashNids :: Prelude.Maybe [Prelude.Text],
    -- | You enable root squash by setting a user ID (UID) and group ID (GID) for
    -- the file system in the format @UID:GID@ (for example, @365534:65534@).
    -- The UID and GID values can range from @0@ to @4294967294@:
    --
    -- -   A non-zero value for UID and GID enables root squash. The UID and
    --     GID values can be different, but each must be a non-zero value.
    --
    -- -   A value of @0@ (zero) for UID and GID indicates root, and therefore
    --     disables root squash.
    --
    -- When root squash is enabled, the user ID and group ID of a root user
    -- accessing the file system are re-mapped to the UID and GID you provide.
    rootSquash :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LustreRootSquashConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'noSquashNids', 'lustreRootSquashConfiguration_noSquashNids' - When root squash is enabled, you can optionally specify an array of NIDs
-- of clients for which root squash does not apply. A client NID is a
-- Lustre Network Identifier used to uniquely identify a client. You can
-- specify the NID as either a single address or a range of addresses:
--
-- -   A single address is described in standard Lustre NID format by
--     specifying the client’s IP address followed by the Lustre network ID
--     (for example, @10.0.1.6\@tcp@).
--
-- -   An address range is described using a dash to separate the range
--     (for example, @10.0.[2-10].[1-255]\@tcp@).
--
-- 'rootSquash', 'lustreRootSquashConfiguration_rootSquash' - You enable root squash by setting a user ID (UID) and group ID (GID) for
-- the file system in the format @UID:GID@ (for example, @365534:65534@).
-- The UID and GID values can range from @0@ to @4294967294@:
--
-- -   A non-zero value for UID and GID enables root squash. The UID and
--     GID values can be different, but each must be a non-zero value.
--
-- -   A value of @0@ (zero) for UID and GID indicates root, and therefore
--     disables root squash.
--
-- When root squash is enabled, the user ID and group ID of a root user
-- accessing the file system are re-mapped to the UID and GID you provide.
newLustreRootSquashConfiguration ::
  LustreRootSquashConfiguration
newLustreRootSquashConfiguration =
  LustreRootSquashConfiguration'
    { noSquashNids =
        Prelude.Nothing,
      rootSquash = Prelude.Nothing
    }

-- | When root squash is enabled, you can optionally specify an array of NIDs
-- of clients for which root squash does not apply. A client NID is a
-- Lustre Network Identifier used to uniquely identify a client. You can
-- specify the NID as either a single address or a range of addresses:
--
-- -   A single address is described in standard Lustre NID format by
--     specifying the client’s IP address followed by the Lustre network ID
--     (for example, @10.0.1.6\@tcp@).
--
-- -   An address range is described using a dash to separate the range
--     (for example, @10.0.[2-10].[1-255]\@tcp@).
lustreRootSquashConfiguration_noSquashNids :: Lens.Lens' LustreRootSquashConfiguration (Prelude.Maybe [Prelude.Text])
lustreRootSquashConfiguration_noSquashNids = Lens.lens (\LustreRootSquashConfiguration' {noSquashNids} -> noSquashNids) (\s@LustreRootSquashConfiguration' {} a -> s {noSquashNids = a} :: LustreRootSquashConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | You enable root squash by setting a user ID (UID) and group ID (GID) for
-- the file system in the format @UID:GID@ (for example, @365534:65534@).
-- The UID and GID values can range from @0@ to @4294967294@:
--
-- -   A non-zero value for UID and GID enables root squash. The UID and
--     GID values can be different, but each must be a non-zero value.
--
-- -   A value of @0@ (zero) for UID and GID indicates root, and therefore
--     disables root squash.
--
-- When root squash is enabled, the user ID and group ID of a root user
-- accessing the file system are re-mapped to the UID and GID you provide.
lustreRootSquashConfiguration_rootSquash :: Lens.Lens' LustreRootSquashConfiguration (Prelude.Maybe Prelude.Text)
lustreRootSquashConfiguration_rootSquash = Lens.lens (\LustreRootSquashConfiguration' {rootSquash} -> rootSquash) (\s@LustreRootSquashConfiguration' {} a -> s {rootSquash = a} :: LustreRootSquashConfiguration)

instance Data.FromJSON LustreRootSquashConfiguration where
  parseJSON =
    Data.withObject
      "LustreRootSquashConfiguration"
      ( \x ->
          LustreRootSquashConfiguration'
            Prelude.<$> (x Data..:? "NoSquashNids" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RootSquash")
      )

instance
  Prelude.Hashable
    LustreRootSquashConfiguration
  where
  hashWithSalt _salt LustreRootSquashConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` noSquashNids
      `Prelude.hashWithSalt` rootSquash

instance Prelude.NFData LustreRootSquashConfiguration where
  rnf LustreRootSquashConfiguration' {..} =
    Prelude.rnf noSquashNids
      `Prelude.seq` Prelude.rnf rootSquash

instance Data.ToJSON LustreRootSquashConfiguration where
  toJSON LustreRootSquashConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NoSquashNids" Data..=) Prelude.<$> noSquashNids,
            ("RootSquash" Data..=) Prelude.<$> rootSquash
          ]
      )
