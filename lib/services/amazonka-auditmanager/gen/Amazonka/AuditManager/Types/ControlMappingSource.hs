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
-- Module      : Amazonka.AuditManager.Types.ControlMappingSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlMappingSource where

import Amazonka.AuditManager.Types.SourceFrequency
import Amazonka.AuditManager.Types.SourceKeyword
import Amazonka.AuditManager.Types.SourceSetUpOption
import Amazonka.AuditManager.Types.SourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The data source that determines from where Audit Manager collects
-- evidence for the control.
--
-- /See:/ 'newControlMappingSource' smart constructor.
data ControlMappingSource = ControlMappingSource'
  { -- | The name of the specified source.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | Specifies one of the five types of data sources for evidence collection.
    sourceType :: Prelude.Maybe SourceType,
    -- | The instructions for troubleshooting the specified control.
    troubleshootingText :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the specified source.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | The description of the specified source.
    sourceDescription :: Prelude.Maybe Prelude.Text,
    -- | The frequency of evidence collection for the specified control mapping
    -- source.
    sourceFrequency :: Prelude.Maybe SourceFrequency,
    sourceKeyword :: Prelude.Maybe SourceKeyword,
    -- | The setup option for the data source, which reflects if the evidence
    -- collection is automated or manual.
    sourceSetUpOption :: Prelude.Maybe SourceSetUpOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlMappingSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceName', 'controlMappingSource_sourceName' - The name of the specified source.
--
-- 'sourceType', 'controlMappingSource_sourceType' - Specifies one of the five types of data sources for evidence collection.
--
-- 'troubleshootingText', 'controlMappingSource_troubleshootingText' - The instructions for troubleshooting the specified control.
--
-- 'sourceId', 'controlMappingSource_sourceId' - The unique identifier for the specified source.
--
-- 'sourceDescription', 'controlMappingSource_sourceDescription' - The description of the specified source.
--
-- 'sourceFrequency', 'controlMappingSource_sourceFrequency' - The frequency of evidence collection for the specified control mapping
-- source.
--
-- 'sourceKeyword', 'controlMappingSource_sourceKeyword' - Undocumented member.
--
-- 'sourceSetUpOption', 'controlMappingSource_sourceSetUpOption' - The setup option for the data source, which reflects if the evidence
-- collection is automated or manual.
newControlMappingSource ::
  ControlMappingSource
newControlMappingSource =
  ControlMappingSource'
    { sourceName = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      troubleshootingText = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      sourceDescription = Prelude.Nothing,
      sourceFrequency = Prelude.Nothing,
      sourceKeyword = Prelude.Nothing,
      sourceSetUpOption = Prelude.Nothing
    }

-- | The name of the specified source.
controlMappingSource_sourceName :: Lens.Lens' ControlMappingSource (Prelude.Maybe Prelude.Text)
controlMappingSource_sourceName = Lens.lens (\ControlMappingSource' {sourceName} -> sourceName) (\s@ControlMappingSource' {} a -> s {sourceName = a} :: ControlMappingSource)

-- | Specifies one of the five types of data sources for evidence collection.
controlMappingSource_sourceType :: Lens.Lens' ControlMappingSource (Prelude.Maybe SourceType)
controlMappingSource_sourceType = Lens.lens (\ControlMappingSource' {sourceType} -> sourceType) (\s@ControlMappingSource' {} a -> s {sourceType = a} :: ControlMappingSource)

-- | The instructions for troubleshooting the specified control.
controlMappingSource_troubleshootingText :: Lens.Lens' ControlMappingSource (Prelude.Maybe Prelude.Text)
controlMappingSource_troubleshootingText = Lens.lens (\ControlMappingSource' {troubleshootingText} -> troubleshootingText) (\s@ControlMappingSource' {} a -> s {troubleshootingText = a} :: ControlMappingSource)

-- | The unique identifier for the specified source.
controlMappingSource_sourceId :: Lens.Lens' ControlMappingSource (Prelude.Maybe Prelude.Text)
controlMappingSource_sourceId = Lens.lens (\ControlMappingSource' {sourceId} -> sourceId) (\s@ControlMappingSource' {} a -> s {sourceId = a} :: ControlMappingSource)

-- | The description of the specified source.
controlMappingSource_sourceDescription :: Lens.Lens' ControlMappingSource (Prelude.Maybe Prelude.Text)
controlMappingSource_sourceDescription = Lens.lens (\ControlMappingSource' {sourceDescription} -> sourceDescription) (\s@ControlMappingSource' {} a -> s {sourceDescription = a} :: ControlMappingSource)

-- | The frequency of evidence collection for the specified control mapping
-- source.
controlMappingSource_sourceFrequency :: Lens.Lens' ControlMappingSource (Prelude.Maybe SourceFrequency)
controlMappingSource_sourceFrequency = Lens.lens (\ControlMappingSource' {sourceFrequency} -> sourceFrequency) (\s@ControlMappingSource' {} a -> s {sourceFrequency = a} :: ControlMappingSource)

-- | Undocumented member.
controlMappingSource_sourceKeyword :: Lens.Lens' ControlMappingSource (Prelude.Maybe SourceKeyword)
controlMappingSource_sourceKeyword = Lens.lens (\ControlMappingSource' {sourceKeyword} -> sourceKeyword) (\s@ControlMappingSource' {} a -> s {sourceKeyword = a} :: ControlMappingSource)

-- | The setup option for the data source, which reflects if the evidence
-- collection is automated or manual.
controlMappingSource_sourceSetUpOption :: Lens.Lens' ControlMappingSource (Prelude.Maybe SourceSetUpOption)
controlMappingSource_sourceSetUpOption = Lens.lens (\ControlMappingSource' {sourceSetUpOption} -> sourceSetUpOption) (\s@ControlMappingSource' {} a -> s {sourceSetUpOption = a} :: ControlMappingSource)

instance Core.FromJSON ControlMappingSource where
  parseJSON =
    Core.withObject
      "ControlMappingSource"
      ( \x ->
          ControlMappingSource'
            Prelude.<$> (x Core..:? "sourceName")
            Prelude.<*> (x Core..:? "sourceType")
            Prelude.<*> (x Core..:? "troubleshootingText")
            Prelude.<*> (x Core..:? "sourceId")
            Prelude.<*> (x Core..:? "sourceDescription")
            Prelude.<*> (x Core..:? "sourceFrequency")
            Prelude.<*> (x Core..:? "sourceKeyword")
            Prelude.<*> (x Core..:? "sourceSetUpOption")
      )

instance Prelude.Hashable ControlMappingSource

instance Prelude.NFData ControlMappingSource

instance Core.ToJSON ControlMappingSource where
  toJSON ControlMappingSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sourceName" Core..=) Prelude.<$> sourceName,
            ("sourceType" Core..=) Prelude.<$> sourceType,
            ("troubleshootingText" Core..=)
              Prelude.<$> troubleshootingText,
            ("sourceId" Core..=) Prelude.<$> sourceId,
            ("sourceDescription" Core..=)
              Prelude.<$> sourceDescription,
            ("sourceFrequency" Core..=)
              Prelude.<$> sourceFrequency,
            ("sourceKeyword" Core..=) Prelude.<$> sourceKeyword,
            ("sourceSetUpOption" Core..=)
              Prelude.<$> sourceSetUpOption
          ]
      )
