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
-- Module      : Amazonka.IoTEvents.Types.IotSiteWiseAssetModelPropertyIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.IotSiteWiseAssetModelPropertyIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The asset model property identifer of the input routed from AWS IoT
-- SiteWise.
--
-- /See:/ 'newIotSiteWiseAssetModelPropertyIdentifier' smart constructor.
data IotSiteWiseAssetModelPropertyIdentifier = IotSiteWiseAssetModelPropertyIdentifier'
  { -- | The ID of the AWS IoT SiteWise asset model.
    assetModelId :: Prelude.Text,
    -- | The ID of the AWS IoT SiteWise asset property.
    propertyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IotSiteWiseAssetModelPropertyIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetModelId', 'iotSiteWiseAssetModelPropertyIdentifier_assetModelId' - The ID of the AWS IoT SiteWise asset model.
--
-- 'propertyId', 'iotSiteWiseAssetModelPropertyIdentifier_propertyId' - The ID of the AWS IoT SiteWise asset property.
newIotSiteWiseAssetModelPropertyIdentifier ::
  -- | 'assetModelId'
  Prelude.Text ->
  -- | 'propertyId'
  Prelude.Text ->
  IotSiteWiseAssetModelPropertyIdentifier
newIotSiteWiseAssetModelPropertyIdentifier
  pAssetModelId_
  pPropertyId_ =
    IotSiteWiseAssetModelPropertyIdentifier'
      { assetModelId =
          pAssetModelId_,
        propertyId = pPropertyId_
      }

-- | The ID of the AWS IoT SiteWise asset model.
iotSiteWiseAssetModelPropertyIdentifier_assetModelId :: Lens.Lens' IotSiteWiseAssetModelPropertyIdentifier Prelude.Text
iotSiteWiseAssetModelPropertyIdentifier_assetModelId = Lens.lens (\IotSiteWiseAssetModelPropertyIdentifier' {assetModelId} -> assetModelId) (\s@IotSiteWiseAssetModelPropertyIdentifier' {} a -> s {assetModelId = a} :: IotSiteWiseAssetModelPropertyIdentifier)

-- | The ID of the AWS IoT SiteWise asset property.
iotSiteWiseAssetModelPropertyIdentifier_propertyId :: Lens.Lens' IotSiteWiseAssetModelPropertyIdentifier Prelude.Text
iotSiteWiseAssetModelPropertyIdentifier_propertyId = Lens.lens (\IotSiteWiseAssetModelPropertyIdentifier' {propertyId} -> propertyId) (\s@IotSiteWiseAssetModelPropertyIdentifier' {} a -> s {propertyId = a} :: IotSiteWiseAssetModelPropertyIdentifier)

instance
  Prelude.Hashable
    IotSiteWiseAssetModelPropertyIdentifier
  where
  hashWithSalt
    _salt
    IotSiteWiseAssetModelPropertyIdentifier' {..} =
      _salt `Prelude.hashWithSalt` assetModelId
        `Prelude.hashWithSalt` propertyId

instance
  Prelude.NFData
    IotSiteWiseAssetModelPropertyIdentifier
  where
  rnf IotSiteWiseAssetModelPropertyIdentifier' {..} =
    Prelude.rnf assetModelId
      `Prelude.seq` Prelude.rnf propertyId

instance
  Data.ToJSON
    IotSiteWiseAssetModelPropertyIdentifier
  where
  toJSON IotSiteWiseAssetModelPropertyIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("assetModelId" Data..= assetModelId),
            Prelude.Just ("propertyId" Data..= propertyId)
          ]
      )
