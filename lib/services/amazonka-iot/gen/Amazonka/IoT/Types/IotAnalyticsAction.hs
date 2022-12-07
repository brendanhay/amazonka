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
-- Module      : Amazonka.IoT.Types.IotAnalyticsAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.IotAnalyticsAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sends message data to an IoT Analytics channel.
--
-- /See:/ 'newIotAnalyticsAction' smart constructor.
data IotAnalyticsAction = IotAnalyticsAction'
  { -- | The ARN of the role which has a policy that grants IoT Analytics
    -- permission to send message data via IoT Analytics
    -- (iotanalytics:BatchPutMessage).
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the IoT Analytics channel to which message data will be
    -- sent.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | (deprecated) The ARN of the IoT Analytics channel to which message data
    -- will be sent.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | Whether to process the action as a batch. The default value is @false@.
    --
    -- When @batchMode@ is @true@ and the rule SQL statement evaluates to an
    -- Array, each Array element is delivered as a separate message when passed
    -- by
    -- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_BatchPutMessage.html BatchPutMessage>
    -- to the IoT Analytics channel. The resulting array can\'t have more than
    -- 100 messages.
    batchMode :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IotAnalyticsAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'iotAnalyticsAction_roleArn' - The ARN of the role which has a policy that grants IoT Analytics
-- permission to send message data via IoT Analytics
-- (iotanalytics:BatchPutMessage).
--
-- 'channelName', 'iotAnalyticsAction_channelName' - The name of the IoT Analytics channel to which message data will be
-- sent.
--
-- 'channelArn', 'iotAnalyticsAction_channelArn' - (deprecated) The ARN of the IoT Analytics channel to which message data
-- will be sent.
--
-- 'batchMode', 'iotAnalyticsAction_batchMode' - Whether to process the action as a batch. The default value is @false@.
--
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an
-- Array, each Array element is delivered as a separate message when passed
-- by
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_BatchPutMessage.html BatchPutMessage>
-- to the IoT Analytics channel. The resulting array can\'t have more than
-- 100 messages.
newIotAnalyticsAction ::
  IotAnalyticsAction
newIotAnalyticsAction =
  IotAnalyticsAction'
    { roleArn = Prelude.Nothing,
      channelName = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      batchMode = Prelude.Nothing
    }

-- | The ARN of the role which has a policy that grants IoT Analytics
-- permission to send message data via IoT Analytics
-- (iotanalytics:BatchPutMessage).
iotAnalyticsAction_roleArn :: Lens.Lens' IotAnalyticsAction (Prelude.Maybe Prelude.Text)
iotAnalyticsAction_roleArn = Lens.lens (\IotAnalyticsAction' {roleArn} -> roleArn) (\s@IotAnalyticsAction' {} a -> s {roleArn = a} :: IotAnalyticsAction)

-- | The name of the IoT Analytics channel to which message data will be
-- sent.
iotAnalyticsAction_channelName :: Lens.Lens' IotAnalyticsAction (Prelude.Maybe Prelude.Text)
iotAnalyticsAction_channelName = Lens.lens (\IotAnalyticsAction' {channelName} -> channelName) (\s@IotAnalyticsAction' {} a -> s {channelName = a} :: IotAnalyticsAction)

-- | (deprecated) The ARN of the IoT Analytics channel to which message data
-- will be sent.
iotAnalyticsAction_channelArn :: Lens.Lens' IotAnalyticsAction (Prelude.Maybe Prelude.Text)
iotAnalyticsAction_channelArn = Lens.lens (\IotAnalyticsAction' {channelArn} -> channelArn) (\s@IotAnalyticsAction' {} a -> s {channelArn = a} :: IotAnalyticsAction)

-- | Whether to process the action as a batch. The default value is @false@.
--
-- When @batchMode@ is @true@ and the rule SQL statement evaluates to an
-- Array, each Array element is delivered as a separate message when passed
-- by
-- <https://docs.aws.amazon.com/iotanalytics/latest/APIReference/API_BatchPutMessage.html BatchPutMessage>
-- to the IoT Analytics channel. The resulting array can\'t have more than
-- 100 messages.
iotAnalyticsAction_batchMode :: Lens.Lens' IotAnalyticsAction (Prelude.Maybe Prelude.Bool)
iotAnalyticsAction_batchMode = Lens.lens (\IotAnalyticsAction' {batchMode} -> batchMode) (\s@IotAnalyticsAction' {} a -> s {batchMode = a} :: IotAnalyticsAction)

instance Data.FromJSON IotAnalyticsAction where
  parseJSON =
    Data.withObject
      "IotAnalyticsAction"
      ( \x ->
          IotAnalyticsAction'
            Prelude.<$> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "channelName")
            Prelude.<*> (x Data..:? "channelArn")
            Prelude.<*> (x Data..:? "batchMode")
      )

instance Prelude.Hashable IotAnalyticsAction where
  hashWithSalt _salt IotAnalyticsAction' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` batchMode

instance Prelude.NFData IotAnalyticsAction where
  rnf IotAnalyticsAction' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf batchMode

instance Data.ToJSON IotAnalyticsAction where
  toJSON IotAnalyticsAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("roleArn" Data..=) Prelude.<$> roleArn,
            ("channelName" Data..=) Prelude.<$> channelName,
            ("channelArn" Data..=) Prelude.<$> channelArn,
            ("batchMode" Data..=) Prelude.<$> batchMode
          ]
      )
