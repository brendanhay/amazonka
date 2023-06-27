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
-- Module      : Amazonka.EC2.Types.NetworkInsightsAccessScopeAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInsightsAccessScopeAnalysis where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AnalysisStatus
import Amazonka.EC2.Types.FindingsFound
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a Network Access Scope analysis.
--
-- /See:/ 'newNetworkInsightsAccessScopeAnalysis' smart constructor.
data NetworkInsightsAccessScopeAnalysis = NetworkInsightsAccessScopeAnalysis'
  { -- | The number of network interfaces analyzed.
    analyzedEniCount :: Prelude.Maybe Prelude.Int,
    -- | The analysis end date.
    endDate :: Prelude.Maybe Data.ISO8601,
    -- | Indicates whether there are findings.
    findingsFound :: Prelude.Maybe FindingsFound,
    -- | The Amazon Resource Name (ARN) of the Network Access Scope analysis.
    networkInsightsAccessScopeAnalysisArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Network Access Scope analysis.
    networkInsightsAccessScopeAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Network Access Scope.
    networkInsightsAccessScopeId :: Prelude.Maybe Prelude.Text,
    -- | The analysis start date.
    startDate :: Prelude.Maybe Data.ISO8601,
    -- | The status.
    status :: Prelude.Maybe AnalysisStatus,
    -- | The status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The warning message.
    warningMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInsightsAccessScopeAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzedEniCount', 'networkInsightsAccessScopeAnalysis_analyzedEniCount' - The number of network interfaces analyzed.
--
-- 'endDate', 'networkInsightsAccessScopeAnalysis_endDate' - The analysis end date.
--
-- 'findingsFound', 'networkInsightsAccessScopeAnalysis_findingsFound' - Indicates whether there are findings.
--
-- 'networkInsightsAccessScopeAnalysisArn', 'networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisArn' - The Amazon Resource Name (ARN) of the Network Access Scope analysis.
--
-- 'networkInsightsAccessScopeAnalysisId', 'networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId' - The ID of the Network Access Scope analysis.
--
-- 'networkInsightsAccessScopeId', 'networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId' - The ID of the Network Access Scope.
--
-- 'startDate', 'networkInsightsAccessScopeAnalysis_startDate' - The analysis start date.
--
-- 'status', 'networkInsightsAccessScopeAnalysis_status' - The status.
--
-- 'statusMessage', 'networkInsightsAccessScopeAnalysis_statusMessage' - The status message.
--
-- 'tags', 'networkInsightsAccessScopeAnalysis_tags' - The tags.
--
-- 'warningMessage', 'networkInsightsAccessScopeAnalysis_warningMessage' - The warning message.
newNetworkInsightsAccessScopeAnalysis ::
  NetworkInsightsAccessScopeAnalysis
newNetworkInsightsAccessScopeAnalysis =
  NetworkInsightsAccessScopeAnalysis'
    { analyzedEniCount =
        Prelude.Nothing,
      endDate = Prelude.Nothing,
      findingsFound = Prelude.Nothing,
      networkInsightsAccessScopeAnalysisArn =
        Prelude.Nothing,
      networkInsightsAccessScopeAnalysisId =
        Prelude.Nothing,
      networkInsightsAccessScopeId =
        Prelude.Nothing,
      startDate = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tags = Prelude.Nothing,
      warningMessage = Prelude.Nothing
    }

-- | The number of network interfaces analyzed.
networkInsightsAccessScopeAnalysis_analyzedEniCount :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.Int)
networkInsightsAccessScopeAnalysis_analyzedEniCount = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {analyzedEniCount} -> analyzedEniCount) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {analyzedEniCount = a} :: NetworkInsightsAccessScopeAnalysis)

-- | The analysis end date.
networkInsightsAccessScopeAnalysis_endDate :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.UTCTime)
networkInsightsAccessScopeAnalysis_endDate = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {endDate} -> endDate) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {endDate = a} :: NetworkInsightsAccessScopeAnalysis) Prelude.. Lens.mapping Data._Time

-- | Indicates whether there are findings.
networkInsightsAccessScopeAnalysis_findingsFound :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe FindingsFound)
networkInsightsAccessScopeAnalysis_findingsFound = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {findingsFound} -> findingsFound) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {findingsFound = a} :: NetworkInsightsAccessScopeAnalysis)

-- | The Amazon Resource Name (ARN) of the Network Access Scope analysis.
networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisArn :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisArn = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {networkInsightsAccessScopeAnalysisArn} -> networkInsightsAccessScopeAnalysisArn) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {networkInsightsAccessScopeAnalysisArn = a} :: NetworkInsightsAccessScopeAnalysis)

-- | The ID of the Network Access Scope analysis.
networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {networkInsightsAccessScopeAnalysisId} -> networkInsightsAccessScopeAnalysisId) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {networkInsightsAccessScopeAnalysisId = a} :: NetworkInsightsAccessScopeAnalysis)

-- | The ID of the Network Access Scope.
networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {networkInsightsAccessScopeId} -> networkInsightsAccessScopeId) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {networkInsightsAccessScopeId = a} :: NetworkInsightsAccessScopeAnalysis)

-- | The analysis start date.
networkInsightsAccessScopeAnalysis_startDate :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.UTCTime)
networkInsightsAccessScopeAnalysis_startDate = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {startDate} -> startDate) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {startDate = a} :: NetworkInsightsAccessScopeAnalysis) Prelude.. Lens.mapping Data._Time

-- | The status.
networkInsightsAccessScopeAnalysis_status :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe AnalysisStatus)
networkInsightsAccessScopeAnalysis_status = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {status} -> status) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {status = a} :: NetworkInsightsAccessScopeAnalysis)

-- | The status message.
networkInsightsAccessScopeAnalysis_statusMessage :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAccessScopeAnalysis_statusMessage = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {statusMessage} -> statusMessage) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {statusMessage = a} :: NetworkInsightsAccessScopeAnalysis)

-- | The tags.
networkInsightsAccessScopeAnalysis_tags :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe [Tag])
networkInsightsAccessScopeAnalysis_tags = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {tags} -> tags) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {tags = a} :: NetworkInsightsAccessScopeAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The warning message.
networkInsightsAccessScopeAnalysis_warningMessage :: Lens.Lens' NetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAccessScopeAnalysis_warningMessage = Lens.lens (\NetworkInsightsAccessScopeAnalysis' {warningMessage} -> warningMessage) (\s@NetworkInsightsAccessScopeAnalysis' {} a -> s {warningMessage = a} :: NetworkInsightsAccessScopeAnalysis)

instance
  Data.FromXML
    NetworkInsightsAccessScopeAnalysis
  where
  parseXML x =
    NetworkInsightsAccessScopeAnalysis'
      Prelude.<$> (x Data..@? "analyzedEniCount")
      Prelude.<*> (x Data..@? "endDate")
      Prelude.<*> (x Data..@? "findingsFound")
      Prelude.<*> (x Data..@? "networkInsightsAccessScopeAnalysisArn")
      Prelude.<*> (x Data..@? "networkInsightsAccessScopeAnalysisId")
      Prelude.<*> (x Data..@? "networkInsightsAccessScopeId")
      Prelude.<*> (x Data..@? "startDate")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "statusMessage")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "warningMessage")

instance
  Prelude.Hashable
    NetworkInsightsAccessScopeAnalysis
  where
  hashWithSalt
    _salt
    NetworkInsightsAccessScopeAnalysis' {..} =
      _salt
        `Prelude.hashWithSalt` analyzedEniCount
        `Prelude.hashWithSalt` endDate
        `Prelude.hashWithSalt` findingsFound
        `Prelude.hashWithSalt` networkInsightsAccessScopeAnalysisArn
        `Prelude.hashWithSalt` networkInsightsAccessScopeAnalysisId
        `Prelude.hashWithSalt` networkInsightsAccessScopeId
        `Prelude.hashWithSalt` startDate
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` statusMessage
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` warningMessage

instance
  Prelude.NFData
    NetworkInsightsAccessScopeAnalysis
  where
  rnf NetworkInsightsAccessScopeAnalysis' {..} =
    Prelude.rnf analyzedEniCount
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf findingsFound
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeAnalysisArn
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeAnalysisId
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeId
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf warningMessage
