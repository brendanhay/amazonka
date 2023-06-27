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
-- Module      : Amazonka.Redshift.Types.AvailabilityZone
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.AvailabilityZone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.SupportedPlatform

-- | Describes an availability zone.
--
-- /See:/ 'newAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | The name of the availability zone.
    name :: Prelude.Maybe Prelude.Text,
    supportedPlatforms :: Prelude.Maybe [SupportedPlatform]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'availabilityZone_name' - The name of the availability zone.
--
-- 'supportedPlatforms', 'availabilityZone_supportedPlatforms' -
newAvailabilityZone ::
  AvailabilityZone
newAvailabilityZone =
  AvailabilityZone'
    { name = Prelude.Nothing,
      supportedPlatforms = Prelude.Nothing
    }

-- | The name of the availability zone.
availabilityZone_name :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_name = Lens.lens (\AvailabilityZone' {name} -> name) (\s@AvailabilityZone' {} a -> s {name = a} :: AvailabilityZone)

availabilityZone_supportedPlatforms :: Lens.Lens' AvailabilityZone (Prelude.Maybe [SupportedPlatform])
availabilityZone_supportedPlatforms = Lens.lens (\AvailabilityZone' {supportedPlatforms} -> supportedPlatforms) (\s@AvailabilityZone' {} a -> s {supportedPlatforms = a} :: AvailabilityZone) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Prelude.<$> (x Data..@? "Name")
      Prelude.<*> ( x
                      Data..@? "SupportedPlatforms"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "SupportedPlatform")
                  )

instance Prelude.Hashable AvailabilityZone where
  hashWithSalt _salt AvailabilityZone' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` supportedPlatforms

instance Prelude.NFData AvailabilityZone where
  rnf AvailabilityZone' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf supportedPlatforms
