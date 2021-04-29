{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Connect.Types.QueueQuickConnectConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QueueQuickConnectConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a queue for a quick connect. The contact flow
-- must be of type Transfer to Queue.
--
-- /See:/ 'newQueueQuickConnectConfig' smart constructor.
data QueueQuickConnectConfig = QueueQuickConnectConfig'
  { -- | The identifier for the queue.
    queueId :: Prelude.Text,
    -- | The identifier of the contact flow.
    contactFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'QueueQuickConnectConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueId', 'queueQuickConnectConfig_queueId' - The identifier for the queue.
--
-- 'contactFlowId', 'queueQuickConnectConfig_contactFlowId' - The identifier of the contact flow.
newQueueQuickConnectConfig ::
  -- | 'queueId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  QueueQuickConnectConfig
newQueueQuickConnectConfig pQueueId_ pContactFlowId_ =
  QueueQuickConnectConfig'
    { queueId = pQueueId_,
      contactFlowId = pContactFlowId_
    }

-- | The identifier for the queue.
queueQuickConnectConfig_queueId :: Lens.Lens' QueueQuickConnectConfig Prelude.Text
queueQuickConnectConfig_queueId = Lens.lens (\QueueQuickConnectConfig' {queueId} -> queueId) (\s@QueueQuickConnectConfig' {} a -> s {queueId = a} :: QueueQuickConnectConfig)

-- | The identifier of the contact flow.
queueQuickConnectConfig_contactFlowId :: Lens.Lens' QueueQuickConnectConfig Prelude.Text
queueQuickConnectConfig_contactFlowId = Lens.lens (\QueueQuickConnectConfig' {contactFlowId} -> contactFlowId) (\s@QueueQuickConnectConfig' {} a -> s {contactFlowId = a} :: QueueQuickConnectConfig)

instance Prelude.FromJSON QueueQuickConnectConfig where
  parseJSON =
    Prelude.withObject
      "QueueQuickConnectConfig"
      ( \x ->
          QueueQuickConnectConfig'
            Prelude.<$> (x Prelude..: "QueueId")
            Prelude.<*> (x Prelude..: "ContactFlowId")
      )

instance Prelude.Hashable QueueQuickConnectConfig

instance Prelude.NFData QueueQuickConnectConfig

instance Prelude.ToJSON QueueQuickConnectConfig where
  toJSON QueueQuickConnectConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("QueueId" Prelude..= queueId),
            Prelude.Just
              ("ContactFlowId" Prelude..= contactFlowId)
          ]
      )
