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
-- Module      : Network.AWS.SSM.Types.OutputSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OutputSource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the source where the association execution details are
-- stored.
--
-- /See:/ 'newOutputSource' smart constructor.
data OutputSource = OutputSource'
  { -- | The ID of the output source, for example the URL of an S3 bucket.
    outputSourceId :: Core.Maybe Core.Text,
    -- | The type of source where the association execution details are stored,
    -- for example, Amazon S3.
    outputSourceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutputSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSourceId', 'outputSource_outputSourceId' - The ID of the output source, for example the URL of an S3 bucket.
--
-- 'outputSourceType', 'outputSource_outputSourceType' - The type of source where the association execution details are stored,
-- for example, Amazon S3.
newOutputSource ::
  OutputSource
newOutputSource =
  OutputSource'
    { outputSourceId = Core.Nothing,
      outputSourceType = Core.Nothing
    }

-- | The ID of the output source, for example the URL of an S3 bucket.
outputSource_outputSourceId :: Lens.Lens' OutputSource (Core.Maybe Core.Text)
outputSource_outputSourceId = Lens.lens (\OutputSource' {outputSourceId} -> outputSourceId) (\s@OutputSource' {} a -> s {outputSourceId = a} :: OutputSource)

-- | The type of source where the association execution details are stored,
-- for example, Amazon S3.
outputSource_outputSourceType :: Lens.Lens' OutputSource (Core.Maybe Core.Text)
outputSource_outputSourceType = Lens.lens (\OutputSource' {outputSourceType} -> outputSourceType) (\s@OutputSource' {} a -> s {outputSourceType = a} :: OutputSource)

instance Core.FromJSON OutputSource where
  parseJSON =
    Core.withObject
      "OutputSource"
      ( \x ->
          OutputSource'
            Core.<$> (x Core..:? "OutputSourceId")
            Core.<*> (x Core..:? "OutputSourceType")
      )

instance Core.Hashable OutputSource

instance Core.NFData OutputSource
