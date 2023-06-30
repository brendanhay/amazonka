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
-- Module      : Amazonka.IoTSiteWise.Types.AssetPropertySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetPropertySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.PropertyNotification
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of a property associated with an asset.
--
-- /See:/ 'newAssetPropertySummary' smart constructor.
data AssetPropertySummary = AssetPropertySummary'
  { -- | The alias that identifies the property, such as an OPC-UA server data
    -- stream path (for example,
    -- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
    -- in the /IoT SiteWise User Guide/.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the composite model that contains the asset property.
    assetCompositeModelId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the property.
    id :: Prelude.Maybe Prelude.Text,
    notification :: Prelude.Maybe PropertyNotification,
    -- | The unit of measure (such as Newtons or RPM) of the asset property.
    unit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetPropertySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'assetPropertySummary_alias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- 'assetCompositeModelId', 'assetPropertySummary_assetCompositeModelId' - The ID of the composite model that contains the asset property.
--
-- 'id', 'assetPropertySummary_id' - The ID of the property.
--
-- 'notification', 'assetPropertySummary_notification' - Undocumented member.
--
-- 'unit', 'assetPropertySummary_unit' - The unit of measure (such as Newtons or RPM) of the asset property.
newAssetPropertySummary ::
  AssetPropertySummary
newAssetPropertySummary =
  AssetPropertySummary'
    { alias = Prelude.Nothing,
      assetCompositeModelId = Prelude.Nothing,
      id = Prelude.Nothing,
      notification = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
assetPropertySummary_alias :: Lens.Lens' AssetPropertySummary (Prelude.Maybe Prelude.Text)
assetPropertySummary_alias = Lens.lens (\AssetPropertySummary' {alias} -> alias) (\s@AssetPropertySummary' {} a -> s {alias = a} :: AssetPropertySummary)

-- | The ID of the composite model that contains the asset property.
assetPropertySummary_assetCompositeModelId :: Lens.Lens' AssetPropertySummary (Prelude.Maybe Prelude.Text)
assetPropertySummary_assetCompositeModelId = Lens.lens (\AssetPropertySummary' {assetCompositeModelId} -> assetCompositeModelId) (\s@AssetPropertySummary' {} a -> s {assetCompositeModelId = a} :: AssetPropertySummary)

-- | The ID of the property.
assetPropertySummary_id :: Lens.Lens' AssetPropertySummary (Prelude.Maybe Prelude.Text)
assetPropertySummary_id = Lens.lens (\AssetPropertySummary' {id} -> id) (\s@AssetPropertySummary' {} a -> s {id = a} :: AssetPropertySummary)

-- | Undocumented member.
assetPropertySummary_notification :: Lens.Lens' AssetPropertySummary (Prelude.Maybe PropertyNotification)
assetPropertySummary_notification = Lens.lens (\AssetPropertySummary' {notification} -> notification) (\s@AssetPropertySummary' {} a -> s {notification = a} :: AssetPropertySummary)

-- | The unit of measure (such as Newtons or RPM) of the asset property.
assetPropertySummary_unit :: Lens.Lens' AssetPropertySummary (Prelude.Maybe Prelude.Text)
assetPropertySummary_unit = Lens.lens (\AssetPropertySummary' {unit} -> unit) (\s@AssetPropertySummary' {} a -> s {unit = a} :: AssetPropertySummary)

instance Data.FromJSON AssetPropertySummary where
  parseJSON =
    Data.withObject
      "AssetPropertySummary"
      ( \x ->
          AssetPropertySummary'
            Prelude.<$> (x Data..:? "alias")
            Prelude.<*> (x Data..:? "assetCompositeModelId")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "notification")
            Prelude.<*> (x Data..:? "unit")
      )

instance Prelude.Hashable AssetPropertySummary where
  hashWithSalt _salt AssetPropertySummary' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` assetCompositeModelId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` unit

instance Prelude.NFData AssetPropertySummary where
  rnf AssetPropertySummary' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf assetCompositeModelId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf notification
      `Prelude.seq` Prelude.rnf unit
