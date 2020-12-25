{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.IotSiteWiseAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.IotSiteWiseAction
  ( IotSiteWiseAction (..),

    -- * Smart constructor
    mkIotSiteWiseAction,

    -- * Lenses
    iswaPutAssetPropertyValueEntries,
    iswaRoleArn,
  )
where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.PutAssetPropertyValueEntry as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action to send data from an MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
--
-- /See:/ 'mkIotSiteWiseAction' smart constructor.
data IotSiteWiseAction = IotSiteWiseAction'
  { -- | A list of asset property value entries.
    putAssetPropertyValueEntries :: Core.NonEmpty Types.PutAssetPropertyValueEntry,
    -- | The ARN of the role that grants AWS IoT permission to send an asset property value to AWS IoTSiteWise. (@"Action": "iotsitewise:BatchPutAssetPropertyValue"@ ). The trust policy can restrict access to specific asset hierarchy paths.
    roleArn :: Types.AwsArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IotSiteWiseAction' value with any optional fields omitted.
mkIotSiteWiseAction ::
  -- | 'putAssetPropertyValueEntries'
  Core.NonEmpty Types.PutAssetPropertyValueEntry ->
  -- | 'roleArn'
  Types.AwsArn ->
  IotSiteWiseAction
mkIotSiteWiseAction putAssetPropertyValueEntries roleArn =
  IotSiteWiseAction' {putAssetPropertyValueEntries, roleArn}

-- | A list of asset property value entries.
--
-- /Note:/ Consider using 'putAssetPropertyValueEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iswaPutAssetPropertyValueEntries :: Lens.Lens' IotSiteWiseAction (Core.NonEmpty Types.PutAssetPropertyValueEntry)
iswaPutAssetPropertyValueEntries = Lens.field @"putAssetPropertyValueEntries"
{-# DEPRECATED iswaPutAssetPropertyValueEntries "Use generic-lens or generic-optics with 'putAssetPropertyValueEntries' instead." #-}

-- | The ARN of the role that grants AWS IoT permission to send an asset property value to AWS IoTSiteWise. (@"Action": "iotsitewise:BatchPutAssetPropertyValue"@ ). The trust policy can restrict access to specific asset hierarchy paths.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iswaRoleArn :: Lens.Lens' IotSiteWiseAction Types.AwsArn
iswaRoleArn = Lens.field @"roleArn"
{-# DEPRECATED iswaRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON IotSiteWiseAction where
  toJSON IotSiteWiseAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "putAssetPropertyValueEntries"
                  Core..= putAssetPropertyValueEntries
              ),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.FromJSON IotSiteWiseAction where
  parseJSON =
    Core.withObject "IotSiteWiseAction" Core.$
      \x ->
        IotSiteWiseAction'
          Core.<$> (x Core..: "putAssetPropertyValueEntries")
          Core.<*> (x Core..: "roleArn")
