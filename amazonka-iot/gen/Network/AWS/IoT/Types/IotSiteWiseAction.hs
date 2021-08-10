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
-- Module      : Network.AWS.IoT.Types.IotSiteWiseAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.IotSiteWiseAction where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.PutAssetPropertyValueEntry
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an action to send data from an MQTT message that triggered the
-- rule to AWS IoT SiteWise asset properties.
--
-- /See:/ 'newIotSiteWiseAction' smart constructor.
data IotSiteWiseAction = IotSiteWiseAction'
  { -- | A list of asset property value entries.
    putAssetPropertyValueEntries :: Prelude.NonEmpty PutAssetPropertyValueEntry,
    -- | The ARN of the role that grants AWS IoT permission to send an asset
    -- property value to AWS IoTSiteWise.
    -- (@\"Action\": \"iotsitewise:BatchPutAssetPropertyValue\"@). The trust
    -- policy can restrict access to specific asset hierarchy paths.
    roleArn :: Prelude.Text
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
-- 'putAssetPropertyValueEntries', 'iotSiteWiseAction_putAssetPropertyValueEntries' - A list of asset property value entries.
--
-- 'roleArn', 'iotSiteWiseAction_roleArn' - The ARN of the role that grants AWS IoT permission to send an asset
-- property value to AWS IoTSiteWise.
-- (@\"Action\": \"iotsitewise:BatchPutAssetPropertyValue\"@). The trust
-- policy can restrict access to specific asset hierarchy paths.
newIotSiteWiseAction ::
  -- | 'putAssetPropertyValueEntries'
  Prelude.NonEmpty PutAssetPropertyValueEntry ->
  -- | 'roleArn'
  Prelude.Text ->
  IotSiteWiseAction
newIotSiteWiseAction
  pPutAssetPropertyValueEntries_
  pRoleArn_ =
    IotSiteWiseAction'
      { putAssetPropertyValueEntries =
          Lens._Coerce Lens.# pPutAssetPropertyValueEntries_,
        roleArn = pRoleArn_
      }

-- | A list of asset property value entries.
iotSiteWiseAction_putAssetPropertyValueEntries :: Lens.Lens' IotSiteWiseAction (Prelude.NonEmpty PutAssetPropertyValueEntry)
iotSiteWiseAction_putAssetPropertyValueEntries = Lens.lens (\IotSiteWiseAction' {putAssetPropertyValueEntries} -> putAssetPropertyValueEntries) (\s@IotSiteWiseAction' {} a -> s {putAssetPropertyValueEntries = a} :: IotSiteWiseAction) Prelude.. Lens._Coerce

-- | The ARN of the role that grants AWS IoT permission to send an asset
-- property value to AWS IoTSiteWise.
-- (@\"Action\": \"iotsitewise:BatchPutAssetPropertyValue\"@). The trust
-- policy can restrict access to specific asset hierarchy paths.
iotSiteWiseAction_roleArn :: Lens.Lens' IotSiteWiseAction Prelude.Text
iotSiteWiseAction_roleArn = Lens.lens (\IotSiteWiseAction' {roleArn} -> roleArn) (\s@IotSiteWiseAction' {} a -> s {roleArn = a} :: IotSiteWiseAction)

instance Core.FromJSON IotSiteWiseAction where
  parseJSON =
    Core.withObject
      "IotSiteWiseAction"
      ( \x ->
          IotSiteWiseAction'
            Prelude.<$> (x Core..: "putAssetPropertyValueEntries")
            Prelude.<*> (x Core..: "roleArn")
      )

instance Prelude.Hashable IotSiteWiseAction

instance Prelude.NFData IotSiteWiseAction

instance Core.ToJSON IotSiteWiseAction where
  toJSON IotSiteWiseAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "putAssetPropertyValueEntries"
                  Core..= putAssetPropertyValueEntries
              ),
            Prelude.Just ("roleArn" Core..= roleArn)
          ]
      )
