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
-- Module      : Network.AWS.MediaLive.Types.PipelineDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.PipelineDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Runtime details of a pipeline when a channel is running.
--
-- /See:/ 'newPipelineDetail' smart constructor.
data PipelineDetail = PipelineDetail'
  { -- | Pipeline ID
    pipelineId :: Prelude.Maybe Prelude.Text,
    -- | The name of the active input attachment currently being ingested by this
    -- pipeline.
    activeInputAttachmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the input switch schedule action that occurred most recently
    -- and that resulted in the switch to the current input attachment for this
    -- pipeline.
    activeInputSwitchActionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PipelineDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'pipelineDetail_pipelineId' - Pipeline ID
--
-- 'activeInputAttachmentName', 'pipelineDetail_activeInputAttachmentName' - The name of the active input attachment currently being ingested by this
-- pipeline.
--
-- 'activeInputSwitchActionName', 'pipelineDetail_activeInputSwitchActionName' - The name of the input switch schedule action that occurred most recently
-- and that resulted in the switch to the current input attachment for this
-- pipeline.
newPipelineDetail ::
  PipelineDetail
newPipelineDetail =
  PipelineDetail'
    { pipelineId = Prelude.Nothing,
      activeInputAttachmentName = Prelude.Nothing,
      activeInputSwitchActionName = Prelude.Nothing
    }

-- | Pipeline ID
pipelineDetail_pipelineId :: Lens.Lens' PipelineDetail (Prelude.Maybe Prelude.Text)
pipelineDetail_pipelineId = Lens.lens (\PipelineDetail' {pipelineId} -> pipelineId) (\s@PipelineDetail' {} a -> s {pipelineId = a} :: PipelineDetail)

-- | The name of the active input attachment currently being ingested by this
-- pipeline.
pipelineDetail_activeInputAttachmentName :: Lens.Lens' PipelineDetail (Prelude.Maybe Prelude.Text)
pipelineDetail_activeInputAttachmentName = Lens.lens (\PipelineDetail' {activeInputAttachmentName} -> activeInputAttachmentName) (\s@PipelineDetail' {} a -> s {activeInputAttachmentName = a} :: PipelineDetail)

-- | The name of the input switch schedule action that occurred most recently
-- and that resulted in the switch to the current input attachment for this
-- pipeline.
pipelineDetail_activeInputSwitchActionName :: Lens.Lens' PipelineDetail (Prelude.Maybe Prelude.Text)
pipelineDetail_activeInputSwitchActionName = Lens.lens (\PipelineDetail' {activeInputSwitchActionName} -> activeInputSwitchActionName) (\s@PipelineDetail' {} a -> s {activeInputSwitchActionName = a} :: PipelineDetail)

instance Prelude.FromJSON PipelineDetail where
  parseJSON =
    Prelude.withObject
      "PipelineDetail"
      ( \x ->
          PipelineDetail'
            Prelude.<$> (x Prelude..:? "pipelineId")
            Prelude.<*> (x Prelude..:? "activeInputAttachmentName")
            Prelude.<*> (x Prelude..:? "activeInputSwitchActionName")
      )

instance Prelude.Hashable PipelineDetail

instance Prelude.NFData PipelineDetail
