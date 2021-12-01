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
-- Module      : Amazonka.AuditManager.Types.CreateControlMappingSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.CreateControlMappingSource where

import Amazonka.AuditManager.Types.SourceFrequency
import Amazonka.AuditManager.Types.SourceKeyword
import Amazonka.AuditManager.Types.SourceSetUpOption
import Amazonka.AuditManager.Types.SourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Control mapping fields that represent the source for evidence
-- collection, along with related parameters and metadata. This does not
-- contain @mappingID@.
--
-- /See:/ 'newCreateControlMappingSource' smart constructor.
data CreateControlMappingSource = CreateControlMappingSource'
  { -- | The name of the control mapping data source.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | Specifies one of the five types of data sources for evidence collection.
    sourceType :: Prelude.Maybe SourceType,
    -- | The instructions for troubleshooting the specified control.
    troubleshootingText :: Prelude.Maybe Prelude.Text,
    -- | The description of the data source that determines from where Audit
    -- Manager collects evidence for the control.
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
-- Create a value of 'CreateControlMappingSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceName', 'createControlMappingSource_sourceName' - The name of the control mapping data source.
--
-- 'sourceType', 'createControlMappingSource_sourceType' - Specifies one of the five types of data sources for evidence collection.
--
-- 'troubleshootingText', 'createControlMappingSource_troubleshootingText' - The instructions for troubleshooting the specified control.
--
-- 'sourceDescription', 'createControlMappingSource_sourceDescription' - The description of the data source that determines from where Audit
-- Manager collects evidence for the control.
--
-- 'sourceFrequency', 'createControlMappingSource_sourceFrequency' - The frequency of evidence collection for the specified control mapping
-- source.
--
-- 'sourceKeyword', 'createControlMappingSource_sourceKeyword' - Undocumented member.
--
-- 'sourceSetUpOption', 'createControlMappingSource_sourceSetUpOption' - The setup option for the data source, which reflects if the evidence
-- collection is automated or manual.
newCreateControlMappingSource ::
  CreateControlMappingSource
newCreateControlMappingSource =
  CreateControlMappingSource'
    { sourceName =
        Prelude.Nothing,
      sourceType = Prelude.Nothing,
      troubleshootingText = Prelude.Nothing,
      sourceDescription = Prelude.Nothing,
      sourceFrequency = Prelude.Nothing,
      sourceKeyword = Prelude.Nothing,
      sourceSetUpOption = Prelude.Nothing
    }

-- | The name of the control mapping data source.
createControlMappingSource_sourceName :: Lens.Lens' CreateControlMappingSource (Prelude.Maybe Prelude.Text)
createControlMappingSource_sourceName = Lens.lens (\CreateControlMappingSource' {sourceName} -> sourceName) (\s@CreateControlMappingSource' {} a -> s {sourceName = a} :: CreateControlMappingSource)

-- | Specifies one of the five types of data sources for evidence collection.
createControlMappingSource_sourceType :: Lens.Lens' CreateControlMappingSource (Prelude.Maybe SourceType)
createControlMappingSource_sourceType = Lens.lens (\CreateControlMappingSource' {sourceType} -> sourceType) (\s@CreateControlMappingSource' {} a -> s {sourceType = a} :: CreateControlMappingSource)

-- | The instructions for troubleshooting the specified control.
createControlMappingSource_troubleshootingText :: Lens.Lens' CreateControlMappingSource (Prelude.Maybe Prelude.Text)
createControlMappingSource_troubleshootingText = Lens.lens (\CreateControlMappingSource' {troubleshootingText} -> troubleshootingText) (\s@CreateControlMappingSource' {} a -> s {troubleshootingText = a} :: CreateControlMappingSource)

-- | The description of the data source that determines from where Audit
-- Manager collects evidence for the control.
createControlMappingSource_sourceDescription :: Lens.Lens' CreateControlMappingSource (Prelude.Maybe Prelude.Text)
createControlMappingSource_sourceDescription = Lens.lens (\CreateControlMappingSource' {sourceDescription} -> sourceDescription) (\s@CreateControlMappingSource' {} a -> s {sourceDescription = a} :: CreateControlMappingSource)

-- | The frequency of evidence collection for the specified control mapping
-- source.
createControlMappingSource_sourceFrequency :: Lens.Lens' CreateControlMappingSource (Prelude.Maybe SourceFrequency)
createControlMappingSource_sourceFrequency = Lens.lens (\CreateControlMappingSource' {sourceFrequency} -> sourceFrequency) (\s@CreateControlMappingSource' {} a -> s {sourceFrequency = a} :: CreateControlMappingSource)

-- | Undocumented member.
createControlMappingSource_sourceKeyword :: Lens.Lens' CreateControlMappingSource (Prelude.Maybe SourceKeyword)
createControlMappingSource_sourceKeyword = Lens.lens (\CreateControlMappingSource' {sourceKeyword} -> sourceKeyword) (\s@CreateControlMappingSource' {} a -> s {sourceKeyword = a} :: CreateControlMappingSource)

-- | The setup option for the data source, which reflects if the evidence
-- collection is automated or manual.
createControlMappingSource_sourceSetUpOption :: Lens.Lens' CreateControlMappingSource (Prelude.Maybe SourceSetUpOption)
createControlMappingSource_sourceSetUpOption = Lens.lens (\CreateControlMappingSource' {sourceSetUpOption} -> sourceSetUpOption) (\s@CreateControlMappingSource' {} a -> s {sourceSetUpOption = a} :: CreateControlMappingSource)

instance Prelude.Hashable CreateControlMappingSource where
  hashWithSalt salt' CreateControlMappingSource' {..} =
    salt' `Prelude.hashWithSalt` sourceSetUpOption
      `Prelude.hashWithSalt` sourceKeyword
      `Prelude.hashWithSalt` sourceFrequency
      `Prelude.hashWithSalt` sourceDescription
      `Prelude.hashWithSalt` troubleshootingText
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` sourceName

instance Prelude.NFData CreateControlMappingSource where
  rnf CreateControlMappingSource' {..} =
    Prelude.rnf sourceName
      `Prelude.seq` Prelude.rnf sourceSetUpOption
      `Prelude.seq` Prelude.rnf sourceKeyword
      `Prelude.seq` Prelude.rnf sourceFrequency
      `Prelude.seq` Prelude.rnf sourceDescription
      `Prelude.seq` Prelude.rnf troubleshootingText
      `Prelude.seq` Prelude.rnf sourceType

instance Core.ToJSON CreateControlMappingSource where
  toJSON CreateControlMappingSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sourceName" Core..=) Prelude.<$> sourceName,
            ("sourceType" Core..=) Prelude.<$> sourceType,
            ("troubleshootingText" Core..=)
              Prelude.<$> troubleshootingText,
            ("sourceDescription" Core..=)
              Prelude.<$> sourceDescription,
            ("sourceFrequency" Core..=)
              Prelude.<$> sourceFrequency,
            ("sourceKeyword" Core..=) Prelude.<$> sourceKeyword,
            ("sourceSetUpOption" Core..=)
              Prelude.<$> sourceSetUpOption
          ]
      )
