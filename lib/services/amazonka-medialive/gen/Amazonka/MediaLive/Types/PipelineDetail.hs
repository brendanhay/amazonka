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
-- Module      : Amazonka.MediaLive.Types.PipelineDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.PipelineDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Runtime details of a pipeline when a channel is running.
--
-- /See:/ 'newPipelineDetail' smart constructor.
data PipelineDetail = PipelineDetail'
  { -- | The name of the active input attachment currently being ingested by this
    -- pipeline.
    activeInputAttachmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the input switch schedule action that occurred most recently
    -- and that resulted in the switch to the current input attachment for this
    -- pipeline.
    activeInputSwitchActionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the motion graphics activate action that occurred most
    -- recently and that resulted in the current graphics URI for this
    -- pipeline.
    activeMotionGraphicsActionName :: Prelude.Maybe Prelude.Text,
    -- | The current URI being used for HTML5 motion graphics for this pipeline.
    activeMotionGraphicsUri :: Prelude.Maybe Prelude.Text,
    -- | Pipeline ID
    pipelineId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeInputAttachmentName', 'pipelineDetail_activeInputAttachmentName' - The name of the active input attachment currently being ingested by this
-- pipeline.
--
-- 'activeInputSwitchActionName', 'pipelineDetail_activeInputSwitchActionName' - The name of the input switch schedule action that occurred most recently
-- and that resulted in the switch to the current input attachment for this
-- pipeline.
--
-- 'activeMotionGraphicsActionName', 'pipelineDetail_activeMotionGraphicsActionName' - The name of the motion graphics activate action that occurred most
-- recently and that resulted in the current graphics URI for this
-- pipeline.
--
-- 'activeMotionGraphicsUri', 'pipelineDetail_activeMotionGraphicsUri' - The current URI being used for HTML5 motion graphics for this pipeline.
--
-- 'pipelineId', 'pipelineDetail_pipelineId' - Pipeline ID
newPipelineDetail ::
  PipelineDetail
newPipelineDetail =
  PipelineDetail'
    { activeInputAttachmentName =
        Prelude.Nothing,
      activeInputSwitchActionName = Prelude.Nothing,
      activeMotionGraphicsActionName = Prelude.Nothing,
      activeMotionGraphicsUri = Prelude.Nothing,
      pipelineId = Prelude.Nothing
    }

-- | The name of the active input attachment currently being ingested by this
-- pipeline.
pipelineDetail_activeInputAttachmentName :: Lens.Lens' PipelineDetail (Prelude.Maybe Prelude.Text)
pipelineDetail_activeInputAttachmentName = Lens.lens (\PipelineDetail' {activeInputAttachmentName} -> activeInputAttachmentName) (\s@PipelineDetail' {} a -> s {activeInputAttachmentName = a} :: PipelineDetail)

-- | The name of the input switch schedule action that occurred most recently
-- and that resulted in the switch to the current input attachment for this
-- pipeline.
pipelineDetail_activeInputSwitchActionName :: Lens.Lens' PipelineDetail (Prelude.Maybe Prelude.Text)
pipelineDetail_activeInputSwitchActionName = Lens.lens (\PipelineDetail' {activeInputSwitchActionName} -> activeInputSwitchActionName) (\s@PipelineDetail' {} a -> s {activeInputSwitchActionName = a} :: PipelineDetail)

-- | The name of the motion graphics activate action that occurred most
-- recently and that resulted in the current graphics URI for this
-- pipeline.
pipelineDetail_activeMotionGraphicsActionName :: Lens.Lens' PipelineDetail (Prelude.Maybe Prelude.Text)
pipelineDetail_activeMotionGraphicsActionName = Lens.lens (\PipelineDetail' {activeMotionGraphicsActionName} -> activeMotionGraphicsActionName) (\s@PipelineDetail' {} a -> s {activeMotionGraphicsActionName = a} :: PipelineDetail)

-- | The current URI being used for HTML5 motion graphics for this pipeline.
pipelineDetail_activeMotionGraphicsUri :: Lens.Lens' PipelineDetail (Prelude.Maybe Prelude.Text)
pipelineDetail_activeMotionGraphicsUri = Lens.lens (\PipelineDetail' {activeMotionGraphicsUri} -> activeMotionGraphicsUri) (\s@PipelineDetail' {} a -> s {activeMotionGraphicsUri = a} :: PipelineDetail)

-- | Pipeline ID
pipelineDetail_pipelineId :: Lens.Lens' PipelineDetail (Prelude.Maybe Prelude.Text)
pipelineDetail_pipelineId = Lens.lens (\PipelineDetail' {pipelineId} -> pipelineId) (\s@PipelineDetail' {} a -> s {pipelineId = a} :: PipelineDetail)

instance Data.FromJSON PipelineDetail where
  parseJSON =
    Data.withObject
      "PipelineDetail"
      ( \x ->
          PipelineDetail'
            Prelude.<$> (x Data..:? "activeInputAttachmentName")
            Prelude.<*> (x Data..:? "activeInputSwitchActionName")
            Prelude.<*> (x Data..:? "activeMotionGraphicsActionName")
            Prelude.<*> (x Data..:? "activeMotionGraphicsUri")
            Prelude.<*> (x Data..:? "pipelineId")
      )

instance Prelude.Hashable PipelineDetail where
  hashWithSalt _salt PipelineDetail' {..} =
    _salt
      `Prelude.hashWithSalt` activeInputAttachmentName
      `Prelude.hashWithSalt` activeInputSwitchActionName
      `Prelude.hashWithSalt` activeMotionGraphicsActionName
      `Prelude.hashWithSalt` activeMotionGraphicsUri
      `Prelude.hashWithSalt` pipelineId

instance Prelude.NFData PipelineDetail where
  rnf PipelineDetail' {..} =
    Prelude.rnf activeInputAttachmentName
      `Prelude.seq` Prelude.rnf activeInputSwitchActionName
      `Prelude.seq` Prelude.rnf activeMotionGraphicsActionName
      `Prelude.seq` Prelude.rnf activeMotionGraphicsUri
      `Prelude.seq` Prelude.rnf pipelineId
