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
-- Module      : Amazonka.GameLift.Types.LocationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.LocationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A remote location where a multi-location fleet can deploy EC2 instances
-- for game hosting.
--
-- /See:/ 'newLocationConfiguration' smart constructor.
data LocationConfiguration = LocationConfiguration'
  { -- | An Amazon Web Services Region code, such as @us-west-2@.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'locationConfiguration_location' - An Amazon Web Services Region code, such as @us-west-2@.
newLocationConfiguration ::
  -- | 'location'
  Prelude.Text ->
  LocationConfiguration
newLocationConfiguration pLocation_ =
  LocationConfiguration' {location = pLocation_}

-- | An Amazon Web Services Region code, such as @us-west-2@.
locationConfiguration_location :: Lens.Lens' LocationConfiguration Prelude.Text
locationConfiguration_location = Lens.lens (\LocationConfiguration' {location} -> location) (\s@LocationConfiguration' {} a -> s {location = a} :: LocationConfiguration)

instance Prelude.Hashable LocationConfiguration where
  hashWithSalt _salt LocationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` location

instance Prelude.NFData LocationConfiguration where
  rnf LocationConfiguration' {..} = Prelude.rnf location

instance Data.ToJSON LocationConfiguration where
  toJSON LocationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Location" Data..= location)]
      )
