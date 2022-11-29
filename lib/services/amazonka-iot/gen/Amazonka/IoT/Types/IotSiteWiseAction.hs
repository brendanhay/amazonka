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
-- Module      : Amazonka.IoT.Types.IotSiteWiseAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.IotSiteWiseAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.PutAssetPropertyValueEntry
import qualified Amazonka.Prelude as Prelude

-- | Describes an action to send data from an MQTT message that triggered the
-- rule to IoT SiteWise asset properties.
--
-- /See:/ 'newIotSiteWiseAction' smart constructor.
data IotSiteWiseAction = IotSiteWiseAction'
  { -- | A list of asset property value entries.
    putAssetPropertyValueEntries :: Prelude.NonEmpty PutAssetPropertyValueEntry,
    -- | The ARN of the role that grants IoT permission to send an asset property
    -- value to IoT SiteWise.
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
-- 'roleArn', 'iotSiteWiseAction_roleArn' - The ARN of the role that grants IoT permission to send an asset property
-- value to IoT SiteWise.
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
          Lens.coerced Lens.# pPutAssetPropertyValueEntries_,
        roleArn = pRoleArn_
      }

-- | A list of asset property value entries.
iotSiteWiseAction_putAssetPropertyValueEntries :: Lens.Lens' IotSiteWiseAction (Prelude.NonEmpty PutAssetPropertyValueEntry)
iotSiteWiseAction_putAssetPropertyValueEntries = Lens.lens (\IotSiteWiseAction' {putAssetPropertyValueEntries} -> putAssetPropertyValueEntries) (\s@IotSiteWiseAction' {} a -> s {putAssetPropertyValueEntries = a} :: IotSiteWiseAction) Prelude.. Lens.coerced

-- | The ARN of the role that grants IoT permission to send an asset property
-- value to IoT SiteWise.
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

instance Prelude.Hashable IotSiteWiseAction where
  hashWithSalt _salt IotSiteWiseAction' {..} =
    _salt
      `Prelude.hashWithSalt` putAssetPropertyValueEntries
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData IotSiteWiseAction where
  rnf IotSiteWiseAction' {..} =
    Prelude.rnf putAssetPropertyValueEntries
      `Prelude.seq` Prelude.rnf roleArn

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
