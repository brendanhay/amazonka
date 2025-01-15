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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlMappingSource where

import Amazonka.AuditManager.Types.SourceFrequency
import Amazonka.AuditManager.Types.SourceKeyword
import Amazonka.AuditManager.Types.SourceSetUpOption
import Amazonka.AuditManager.Types.SourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The data source that determines where Audit Manager collects evidence
-- from for the control.
--
-- /See:/ 'newControlMappingSource' smart constructor.
data ControlMappingSource = ControlMappingSource'
  { -- | The description of the source.
    sourceDescription :: Prelude.Maybe Prelude.Text,
    -- | The frequency of evidence collection for the control mapping source.
    sourceFrequency :: Prelude.Maybe SourceFrequency,
    -- | The unique identifier for the source.
    sourceId :: Prelude.Maybe Prelude.Text,
    sourceKeyword :: Prelude.Maybe SourceKeyword,
    -- | The name of the source.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | The setup option for the data source. This option reflects if the
    -- evidence collection is automated or manual.
    sourceSetUpOption :: Prelude.Maybe SourceSetUpOption,
    -- | Specifies one of the five data source types for evidence collection.
    sourceType :: Prelude.Maybe SourceType,
    -- | The instructions for troubleshooting the control.
    troubleshootingText :: Prelude.Maybe Prelude.Text
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
-- 'sourceDescription', 'controlMappingSource_sourceDescription' - The description of the source.
--
-- 'sourceFrequency', 'controlMappingSource_sourceFrequency' - The frequency of evidence collection for the control mapping source.
--
-- 'sourceId', 'controlMappingSource_sourceId' - The unique identifier for the source.
--
-- 'sourceKeyword', 'controlMappingSource_sourceKeyword' - Undocumented member.
--
-- 'sourceName', 'controlMappingSource_sourceName' - The name of the source.
--
-- 'sourceSetUpOption', 'controlMappingSource_sourceSetUpOption' - The setup option for the data source. This option reflects if the
-- evidence collection is automated or manual.
--
-- 'sourceType', 'controlMappingSource_sourceType' - Specifies one of the five data source types for evidence collection.
--
-- 'troubleshootingText', 'controlMappingSource_troubleshootingText' - The instructions for troubleshooting the control.
newControlMappingSource ::
  ControlMappingSource
newControlMappingSource =
  ControlMappingSource'
    { sourceDescription =
        Prelude.Nothing,
      sourceFrequency = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      sourceKeyword = Prelude.Nothing,
      sourceName = Prelude.Nothing,
      sourceSetUpOption = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      troubleshootingText = Prelude.Nothing
    }

-- | The description of the source.
controlMappingSource_sourceDescription :: Lens.Lens' ControlMappingSource (Prelude.Maybe Prelude.Text)
controlMappingSource_sourceDescription = Lens.lens (\ControlMappingSource' {sourceDescription} -> sourceDescription) (\s@ControlMappingSource' {} a -> s {sourceDescription = a} :: ControlMappingSource)

-- | The frequency of evidence collection for the control mapping source.
controlMappingSource_sourceFrequency :: Lens.Lens' ControlMappingSource (Prelude.Maybe SourceFrequency)
controlMappingSource_sourceFrequency = Lens.lens (\ControlMappingSource' {sourceFrequency} -> sourceFrequency) (\s@ControlMappingSource' {} a -> s {sourceFrequency = a} :: ControlMappingSource)

-- | The unique identifier for the source.
controlMappingSource_sourceId :: Lens.Lens' ControlMappingSource (Prelude.Maybe Prelude.Text)
controlMappingSource_sourceId = Lens.lens (\ControlMappingSource' {sourceId} -> sourceId) (\s@ControlMappingSource' {} a -> s {sourceId = a} :: ControlMappingSource)

-- | Undocumented member.
controlMappingSource_sourceKeyword :: Lens.Lens' ControlMappingSource (Prelude.Maybe SourceKeyword)
controlMappingSource_sourceKeyword = Lens.lens (\ControlMappingSource' {sourceKeyword} -> sourceKeyword) (\s@ControlMappingSource' {} a -> s {sourceKeyword = a} :: ControlMappingSource)

-- | The name of the source.
controlMappingSource_sourceName :: Lens.Lens' ControlMappingSource (Prelude.Maybe Prelude.Text)
controlMappingSource_sourceName = Lens.lens (\ControlMappingSource' {sourceName} -> sourceName) (\s@ControlMappingSource' {} a -> s {sourceName = a} :: ControlMappingSource)

-- | The setup option for the data source. This option reflects if the
-- evidence collection is automated or manual.
controlMappingSource_sourceSetUpOption :: Lens.Lens' ControlMappingSource (Prelude.Maybe SourceSetUpOption)
controlMappingSource_sourceSetUpOption = Lens.lens (\ControlMappingSource' {sourceSetUpOption} -> sourceSetUpOption) (\s@ControlMappingSource' {} a -> s {sourceSetUpOption = a} :: ControlMappingSource)

-- | Specifies one of the five data source types for evidence collection.
controlMappingSource_sourceType :: Lens.Lens' ControlMappingSource (Prelude.Maybe SourceType)
controlMappingSource_sourceType = Lens.lens (\ControlMappingSource' {sourceType} -> sourceType) (\s@ControlMappingSource' {} a -> s {sourceType = a} :: ControlMappingSource)

-- | The instructions for troubleshooting the control.
controlMappingSource_troubleshootingText :: Lens.Lens' ControlMappingSource (Prelude.Maybe Prelude.Text)
controlMappingSource_troubleshootingText = Lens.lens (\ControlMappingSource' {troubleshootingText} -> troubleshootingText) (\s@ControlMappingSource' {} a -> s {troubleshootingText = a} :: ControlMappingSource)

instance Data.FromJSON ControlMappingSource where
  parseJSON =
    Data.withObject
      "ControlMappingSource"
      ( \x ->
          ControlMappingSource'
            Prelude.<$> (x Data..:? "sourceDescription")
            Prelude.<*> (x Data..:? "sourceFrequency")
            Prelude.<*> (x Data..:? "sourceId")
            Prelude.<*> (x Data..:? "sourceKeyword")
            Prelude.<*> (x Data..:? "sourceName")
            Prelude.<*> (x Data..:? "sourceSetUpOption")
            Prelude.<*> (x Data..:? "sourceType")
            Prelude.<*> (x Data..:? "troubleshootingText")
      )

instance Prelude.Hashable ControlMappingSource where
  hashWithSalt _salt ControlMappingSource' {..} =
    _salt
      `Prelude.hashWithSalt` sourceDescription
      `Prelude.hashWithSalt` sourceFrequency
      `Prelude.hashWithSalt` sourceId
      `Prelude.hashWithSalt` sourceKeyword
      `Prelude.hashWithSalt` sourceName
      `Prelude.hashWithSalt` sourceSetUpOption
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` troubleshootingText

instance Prelude.NFData ControlMappingSource where
  rnf ControlMappingSource' {..} =
    Prelude.rnf sourceDescription `Prelude.seq`
      Prelude.rnf sourceFrequency `Prelude.seq`
        Prelude.rnf sourceId `Prelude.seq`
          Prelude.rnf sourceKeyword `Prelude.seq`
            Prelude.rnf sourceName `Prelude.seq`
              Prelude.rnf sourceSetUpOption `Prelude.seq`
                Prelude.rnf sourceType `Prelude.seq`
                  Prelude.rnf troubleshootingText

instance Data.ToJSON ControlMappingSource where
  toJSON ControlMappingSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sourceDescription" Data..=)
              Prelude.<$> sourceDescription,
            ("sourceFrequency" Data..=)
              Prelude.<$> sourceFrequency,
            ("sourceId" Data..=) Prelude.<$> sourceId,
            ("sourceKeyword" Data..=) Prelude.<$> sourceKeyword,
            ("sourceName" Data..=) Prelude.<$> sourceName,
            ("sourceSetUpOption" Data..=)
              Prelude.<$> sourceSetUpOption,
            ("sourceType" Data..=) Prelude.<$> sourceType,
            ("troubleshootingText" Data..=)
              Prelude.<$> troubleshootingText
          ]
      )
