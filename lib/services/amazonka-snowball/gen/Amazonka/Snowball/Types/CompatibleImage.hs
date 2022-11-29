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
-- Module      : Amazonka.Snowball.Types.CompatibleImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.CompatibleImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A JSON-formatted object that describes a compatible Amazon Machine Image
-- (AMI), including the ID and name for a Snow device AMI. This AMI is
-- compatible with the device\'s physical hardware requirements, and it
-- should be able to be run in an SBE1 instance on the device.
--
-- /See:/ 'newCompatibleImage' smart constructor.
data CompatibleImage = CompatibleImage'
  { -- | The unique identifier for an individual Snow device AMI.
    amiId :: Prelude.Maybe Prelude.Text,
    -- | The optional name of a compatible image.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompatibleImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amiId', 'compatibleImage_amiId' - The unique identifier for an individual Snow device AMI.
--
-- 'name', 'compatibleImage_name' - The optional name of a compatible image.
newCompatibleImage ::
  CompatibleImage
newCompatibleImage =
  CompatibleImage'
    { amiId = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The unique identifier for an individual Snow device AMI.
compatibleImage_amiId :: Lens.Lens' CompatibleImage (Prelude.Maybe Prelude.Text)
compatibleImage_amiId = Lens.lens (\CompatibleImage' {amiId} -> amiId) (\s@CompatibleImage' {} a -> s {amiId = a} :: CompatibleImage)

-- | The optional name of a compatible image.
compatibleImage_name :: Lens.Lens' CompatibleImage (Prelude.Maybe Prelude.Text)
compatibleImage_name = Lens.lens (\CompatibleImage' {name} -> name) (\s@CompatibleImage' {} a -> s {name = a} :: CompatibleImage)

instance Core.FromJSON CompatibleImage where
  parseJSON =
    Core.withObject
      "CompatibleImage"
      ( \x ->
          CompatibleImage'
            Prelude.<$> (x Core..:? "AmiId") Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable CompatibleImage where
  hashWithSalt _salt CompatibleImage' {..} =
    _salt `Prelude.hashWithSalt` amiId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CompatibleImage where
  rnf CompatibleImage' {..} =
    Prelude.rnf amiId `Prelude.seq` Prelude.rnf name
