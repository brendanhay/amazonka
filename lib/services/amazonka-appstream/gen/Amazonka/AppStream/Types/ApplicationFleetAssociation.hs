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
-- Module      : Amazonka.AppStream.Types.ApplicationFleetAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ApplicationFleetAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the application fleet association.
--
-- /See:/ 'newApplicationFleetAssociation' smart constructor.
data ApplicationFleetAssociation = ApplicationFleetAssociation'
  { -- | The name of the fleet associated with the application.
    fleetName :: Prelude.Text,
    -- | The ARN of the application associated with the fleet.
    applicationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationFleetAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetName', 'applicationFleetAssociation_fleetName' - The name of the fleet associated with the application.
--
-- 'applicationArn', 'applicationFleetAssociation_applicationArn' - The ARN of the application associated with the fleet.
newApplicationFleetAssociation ::
  -- | 'fleetName'
  Prelude.Text ->
  -- | 'applicationArn'
  Prelude.Text ->
  ApplicationFleetAssociation
newApplicationFleetAssociation
  pFleetName_
  pApplicationArn_ =
    ApplicationFleetAssociation'
      { fleetName =
          pFleetName_,
        applicationArn = pApplicationArn_
      }

-- | The name of the fleet associated with the application.
applicationFleetAssociation_fleetName :: Lens.Lens' ApplicationFleetAssociation Prelude.Text
applicationFleetAssociation_fleetName = Lens.lens (\ApplicationFleetAssociation' {fleetName} -> fleetName) (\s@ApplicationFleetAssociation' {} a -> s {fleetName = a} :: ApplicationFleetAssociation)

-- | The ARN of the application associated with the fleet.
applicationFleetAssociation_applicationArn :: Lens.Lens' ApplicationFleetAssociation Prelude.Text
applicationFleetAssociation_applicationArn = Lens.lens (\ApplicationFleetAssociation' {applicationArn} -> applicationArn) (\s@ApplicationFleetAssociation' {} a -> s {applicationArn = a} :: ApplicationFleetAssociation)

instance Data.FromJSON ApplicationFleetAssociation where
  parseJSON =
    Data.withObject
      "ApplicationFleetAssociation"
      ( \x ->
          ApplicationFleetAssociation'
            Prelude.<$> (x Data..: "FleetName")
            Prelude.<*> (x Data..: "ApplicationArn")
      )

instance Prelude.Hashable ApplicationFleetAssociation where
  hashWithSalt _salt ApplicationFleetAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` fleetName
      `Prelude.hashWithSalt` applicationArn

instance Prelude.NFData ApplicationFleetAssociation where
  rnf ApplicationFleetAssociation' {..} =
    Prelude.rnf fleetName
      `Prelude.seq` Prelude.rnf applicationArn
