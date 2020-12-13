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
    iswaRoleARN,
  )
where

import Network.AWS.IoT.Types.PutAssetPropertyValueEntry
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action to send data from an MQTT message that triggered the rule to AWS IoT SiteWise asset properties.
--
-- /See:/ 'mkIotSiteWiseAction' smart constructor.
data IotSiteWiseAction = IotSiteWiseAction'
  { -- | A list of asset property value entries.
    putAssetPropertyValueEntries :: Lude.NonEmpty PutAssetPropertyValueEntry,
    -- | The ARN of the role that grants AWS IoT permission to send an asset property value to AWS IoTSiteWise. (@"Action": "iotsitewise:BatchPutAssetPropertyValue"@ ). The trust policy can restrict access to specific asset hierarchy paths.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IotSiteWiseAction' with the minimum fields required to make a request.
--
-- * 'putAssetPropertyValueEntries' - A list of asset property value entries.
-- * 'roleARN' - The ARN of the role that grants AWS IoT permission to send an asset property value to AWS IoTSiteWise. (@"Action": "iotsitewise:BatchPutAssetPropertyValue"@ ). The trust policy can restrict access to specific asset hierarchy paths.
mkIotSiteWiseAction ::
  -- | 'putAssetPropertyValueEntries'
  Lude.NonEmpty PutAssetPropertyValueEntry ->
  -- | 'roleARN'
  Lude.Text ->
  IotSiteWiseAction
mkIotSiteWiseAction pPutAssetPropertyValueEntries_ pRoleARN_ =
  IotSiteWiseAction'
    { putAssetPropertyValueEntries =
        pPutAssetPropertyValueEntries_,
      roleARN = pRoleARN_
    }

-- | A list of asset property value entries.
--
-- /Note:/ Consider using 'putAssetPropertyValueEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iswaPutAssetPropertyValueEntries :: Lens.Lens' IotSiteWiseAction (Lude.NonEmpty PutAssetPropertyValueEntry)
iswaPutAssetPropertyValueEntries = Lens.lens (putAssetPropertyValueEntries :: IotSiteWiseAction -> Lude.NonEmpty PutAssetPropertyValueEntry) (\s a -> s {putAssetPropertyValueEntries = a} :: IotSiteWiseAction)
{-# DEPRECATED iswaPutAssetPropertyValueEntries "Use generic-lens or generic-optics with 'putAssetPropertyValueEntries' instead." #-}

-- | The ARN of the role that grants AWS IoT permission to send an asset property value to AWS IoTSiteWise. (@"Action": "iotsitewise:BatchPutAssetPropertyValue"@ ). The trust policy can restrict access to specific asset hierarchy paths.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iswaRoleARN :: Lens.Lens' IotSiteWiseAction Lude.Text
iswaRoleARN = Lens.lens (roleARN :: IotSiteWiseAction -> Lude.Text) (\s a -> s {roleARN = a} :: IotSiteWiseAction)
{-# DEPRECATED iswaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON IotSiteWiseAction where
  parseJSON =
    Lude.withObject
      "IotSiteWiseAction"
      ( \x ->
          IotSiteWiseAction'
            Lude.<$> (x Lude..: "putAssetPropertyValueEntries")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON IotSiteWiseAction where
  toJSON IotSiteWiseAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "putAssetPropertyValueEntries"
                  Lude..= putAssetPropertyValueEntries
              ),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
