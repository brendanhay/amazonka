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
-- Module      : Amazonka.IoTEvents.Types.IotSiteWiseAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.IotSiteWiseAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.AssetPropertyValue
import qualified Amazonka.Prelude as Prelude

-- | Sends information about the detector model instance and the event that
-- triggered the action to a specified asset property in AWS IoT SiteWise.
--
-- You must use expressions for all parameters in @IotSiteWiseAction@. The
-- expressions accept literals, operators, functions, references, and
-- substitutions templates.
--
-- __Examples__
--
-- -   For literal values, the expressions must contain single quotes. For
--     example, the value for the @propertyAlias@ parameter can be
--     @\'\/company\/windfarm\/3\/turbine\/7\/temperature\'@.
--
-- -   For references, you must specify either variables or input values.
--     For example, the value for the @assetId@ parameter can be
--     @$input.TurbineInput.assetId1@.
--
-- -   For a substitution template, you must use @${}@, and the template
--     must be in single quotes. A substitution template can also contain a
--     combination of literals, operators, functions, references, and
--     substitution templates.
--
--     In the following example, the value for the @propertyAlias@
--     parameter uses a substitution template.
--
--     @\'company\/windfarm\/${$input.TemperatureInput.sensorData.windfarmID}\/turbine\/ ${$input.TemperatureInput.sensorData.turbineID}\/temperature\'@
--
-- You must specify either @propertyAlias@ or both @assetId@ and
-- @propertyId@ to identify the target asset property in AWS IoT SiteWise.
--
-- For more information, see
-- <https://docs.aws.amazon.com/iotevents/latest/developerguide/iotevents-expressions.html Expressions>
-- in the /AWS IoT Events Developer Guide/.
--
-- /See:/ 'newIotSiteWiseAction' smart constructor.
data IotSiteWiseAction = IotSiteWiseAction'
  { -- | The ID of the asset that has the specified property.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for this entry. You can use the entry ID to track
    -- which data entry causes an error in case of failure. The default is a
    -- new unique identifier.
    entryId :: Prelude.Maybe Prelude.Text,
    -- | The alias of the asset property.
    propertyAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The value to send to the asset property. This value contains timestamp,
    -- quality, and value (TQV) information.
    propertyValue :: Prelude.Maybe AssetPropertyValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IotSiteWiseAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetId', 'iotSiteWiseAction_assetId' - The ID of the asset that has the specified property.
--
-- 'entryId', 'iotSiteWiseAction_entryId' - A unique identifier for this entry. You can use the entry ID to track
-- which data entry causes an error in case of failure. The default is a
-- new unique identifier.
--
-- 'propertyAlias', 'iotSiteWiseAction_propertyAlias' - The alias of the asset property.
--
-- 'propertyId', 'iotSiteWiseAction_propertyId' - The ID of the asset property.
--
-- 'propertyValue', 'iotSiteWiseAction_propertyValue' - The value to send to the asset property. This value contains timestamp,
-- quality, and value (TQV) information.
newIotSiteWiseAction ::
  IotSiteWiseAction
newIotSiteWiseAction =
  IotSiteWiseAction'
    { assetId = Prelude.Nothing,
      entryId = Prelude.Nothing,
      propertyAlias = Prelude.Nothing,
      propertyId = Prelude.Nothing,
      propertyValue = Prelude.Nothing
    }

-- | The ID of the asset that has the specified property.
iotSiteWiseAction_assetId :: Lens.Lens' IotSiteWiseAction (Prelude.Maybe Prelude.Text)
iotSiteWiseAction_assetId = Lens.lens (\IotSiteWiseAction' {assetId} -> assetId) (\s@IotSiteWiseAction' {} a -> s {assetId = a} :: IotSiteWiseAction)

-- | A unique identifier for this entry. You can use the entry ID to track
-- which data entry causes an error in case of failure. The default is a
-- new unique identifier.
iotSiteWiseAction_entryId :: Lens.Lens' IotSiteWiseAction (Prelude.Maybe Prelude.Text)
iotSiteWiseAction_entryId = Lens.lens (\IotSiteWiseAction' {entryId} -> entryId) (\s@IotSiteWiseAction' {} a -> s {entryId = a} :: IotSiteWiseAction)

-- | The alias of the asset property.
iotSiteWiseAction_propertyAlias :: Lens.Lens' IotSiteWiseAction (Prelude.Maybe Prelude.Text)
iotSiteWiseAction_propertyAlias = Lens.lens (\IotSiteWiseAction' {propertyAlias} -> propertyAlias) (\s@IotSiteWiseAction' {} a -> s {propertyAlias = a} :: IotSiteWiseAction)

-- | The ID of the asset property.
iotSiteWiseAction_propertyId :: Lens.Lens' IotSiteWiseAction (Prelude.Maybe Prelude.Text)
iotSiteWiseAction_propertyId = Lens.lens (\IotSiteWiseAction' {propertyId} -> propertyId) (\s@IotSiteWiseAction' {} a -> s {propertyId = a} :: IotSiteWiseAction)

-- | The value to send to the asset property. This value contains timestamp,
-- quality, and value (TQV) information.
iotSiteWiseAction_propertyValue :: Lens.Lens' IotSiteWiseAction (Prelude.Maybe AssetPropertyValue)
iotSiteWiseAction_propertyValue = Lens.lens (\IotSiteWiseAction' {propertyValue} -> propertyValue) (\s@IotSiteWiseAction' {} a -> s {propertyValue = a} :: IotSiteWiseAction)

instance Data.FromJSON IotSiteWiseAction where
  parseJSON =
    Data.withObject
      "IotSiteWiseAction"
      ( \x ->
          IotSiteWiseAction'
            Prelude.<$> (x Data..:? "assetId")
            Prelude.<*> (x Data..:? "entryId")
            Prelude.<*> (x Data..:? "propertyAlias")
            Prelude.<*> (x Data..:? "propertyId")
            Prelude.<*> (x Data..:? "propertyValue")
      )

instance Prelude.Hashable IotSiteWiseAction where
  hashWithSalt _salt IotSiteWiseAction' {..} =
    _salt
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` entryId
      `Prelude.hashWithSalt` propertyAlias
      `Prelude.hashWithSalt` propertyId
      `Prelude.hashWithSalt` propertyValue

instance Prelude.NFData IotSiteWiseAction where
  rnf IotSiteWiseAction' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf entryId
      `Prelude.seq` Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf propertyId
      `Prelude.seq` Prelude.rnf propertyValue

instance Data.ToJSON IotSiteWiseAction where
  toJSON IotSiteWiseAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assetId" Data..=) Prelude.<$> assetId,
            ("entryId" Data..=) Prelude.<$> entryId,
            ("propertyAlias" Data..=) Prelude.<$> propertyAlias,
            ("propertyId" Data..=) Prelude.<$> propertyId,
            ("propertyValue" Data..=) Prelude.<$> propertyValue
          ]
      )
