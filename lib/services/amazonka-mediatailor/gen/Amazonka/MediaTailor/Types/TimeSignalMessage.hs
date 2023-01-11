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
-- Module      : Amazonka.MediaTailor.Types.TimeSignalMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.TimeSignalMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.SegmentationDescriptor
import qualified Amazonka.Prelude as Prelude

-- | The SCTE-35 @time_signal@ message can be sent with one or more
-- @segmentation_descriptor@ messages. A @time_signal@ message can be sent
-- only if a single @segmentation_descriptor@ message is sent.
--
-- The @time_signal@ message contains only the @splice_time@ field which is
-- constructed using a given presentation timestamp. When sending a
-- @time_signal@ message, the @splice_command_type@ field in the
-- @splice_info_section@ message is set to 6 (0x06).
--
-- See the @time_signal()@ table of the 2022 SCTE-35 specification for more
-- information.
--
-- /See:/ 'newTimeSignalMessage' smart constructor.
data TimeSignalMessage = TimeSignalMessage'
  { -- | The configurations for the SCTE-35 @segmentation_descriptor@ message(s)
    -- sent with the @time_signal@ message.
    segmentationDescriptors :: Prelude.Maybe [SegmentationDescriptor]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSignalMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentationDescriptors', 'timeSignalMessage_segmentationDescriptors' - The configurations for the SCTE-35 @segmentation_descriptor@ message(s)
-- sent with the @time_signal@ message.
newTimeSignalMessage ::
  TimeSignalMessage
newTimeSignalMessage =
  TimeSignalMessage'
    { segmentationDescriptors =
        Prelude.Nothing
    }

-- | The configurations for the SCTE-35 @segmentation_descriptor@ message(s)
-- sent with the @time_signal@ message.
timeSignalMessage_segmentationDescriptors :: Lens.Lens' TimeSignalMessage (Prelude.Maybe [SegmentationDescriptor])
timeSignalMessage_segmentationDescriptors = Lens.lens (\TimeSignalMessage' {segmentationDescriptors} -> segmentationDescriptors) (\s@TimeSignalMessage' {} a -> s {segmentationDescriptors = a} :: TimeSignalMessage) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TimeSignalMessage where
  parseJSON =
    Data.withObject
      "TimeSignalMessage"
      ( \x ->
          TimeSignalMessage'
            Prelude.<$> ( x Data..:? "SegmentationDescriptors"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TimeSignalMessage where
  hashWithSalt _salt TimeSignalMessage' {..} =
    _salt
      `Prelude.hashWithSalt` segmentationDescriptors

instance Prelude.NFData TimeSignalMessage where
  rnf TimeSignalMessage' {..} =
    Prelude.rnf segmentationDescriptors

instance Data.ToJSON TimeSignalMessage where
  toJSON TimeSignalMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SegmentationDescriptors" Data..=)
              Prelude.<$> segmentationDescriptors
          ]
      )
