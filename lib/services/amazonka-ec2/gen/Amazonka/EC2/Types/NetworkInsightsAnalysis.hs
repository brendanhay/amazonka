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
-- Module      : Amazonka.EC2.Types.NetworkInsightsAnalysis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInsightsAnalysis where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AlternatePathHint
import Amazonka.EC2.Types.AnalysisStatus
import Amazonka.EC2.Types.Explanation
import Amazonka.EC2.Types.PathComponent
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a network insights analysis.
--
-- /See:/ 'newNetworkInsightsAnalysis' smart constructor.
data NetworkInsightsAnalysis = NetworkInsightsAnalysis'
  { -- | The member accounts that contain resources that the path can traverse.
    additionalAccounts :: Prelude.Maybe [Prelude.Text],
    -- | Potential intermediate components.
    alternatePathHints :: Prelude.Maybe [AlternatePathHint],
    -- | The explanations. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
    explanations :: Prelude.Maybe [Explanation],
    -- | The Amazon Resource Names (ARN) of the Amazon Web Services resources
    -- that the path must traverse.
    filterInArns :: Prelude.Maybe [Prelude.Text],
    -- | The components in the path from source to destination.
    forwardPathComponents :: Prelude.Maybe [PathComponent],
    -- | The Amazon Resource Name (ARN) of the network insights analysis.
    networkInsightsAnalysisArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network insights analysis.
    networkInsightsAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the path.
    networkInsightsPathId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the destination is reachable from the source.
    networkPathFound :: Prelude.Maybe Prelude.Bool,
    -- | The components in the path from destination to source.
    returnPathComponents :: Prelude.Maybe [PathComponent],
    -- | The time the analysis started.
    startDate :: Prelude.Maybe Data.ISO8601,
    -- | The status of the network insights analysis.
    status :: Prelude.Maybe AnalysisStatus,
    -- | The status message, if the status is @failed@.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Potential intermediate accounts.
    suggestedAccounts :: Prelude.Maybe [Prelude.Text],
    -- | The tags.
    tags :: Prelude.Maybe [Tag],
    -- | The warning message.
    warningMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInsightsAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalAccounts', 'networkInsightsAnalysis_additionalAccounts' - The member accounts that contain resources that the path can traverse.
--
-- 'alternatePathHints', 'networkInsightsAnalysis_alternatePathHints' - Potential intermediate components.
--
-- 'explanations', 'networkInsightsAnalysis_explanations' - The explanations. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
--
-- 'filterInArns', 'networkInsightsAnalysis_filterInArns' - The Amazon Resource Names (ARN) of the Amazon Web Services resources
-- that the path must traverse.
--
-- 'forwardPathComponents', 'networkInsightsAnalysis_forwardPathComponents' - The components in the path from source to destination.
--
-- 'networkInsightsAnalysisArn', 'networkInsightsAnalysis_networkInsightsAnalysisArn' - The Amazon Resource Name (ARN) of the network insights analysis.
--
-- 'networkInsightsAnalysisId', 'networkInsightsAnalysis_networkInsightsAnalysisId' - The ID of the network insights analysis.
--
-- 'networkInsightsPathId', 'networkInsightsAnalysis_networkInsightsPathId' - The ID of the path.
--
-- 'networkPathFound', 'networkInsightsAnalysis_networkPathFound' - Indicates whether the destination is reachable from the source.
--
-- 'returnPathComponents', 'networkInsightsAnalysis_returnPathComponents' - The components in the path from destination to source.
--
-- 'startDate', 'networkInsightsAnalysis_startDate' - The time the analysis started.
--
-- 'status', 'networkInsightsAnalysis_status' - The status of the network insights analysis.
--
-- 'statusMessage', 'networkInsightsAnalysis_statusMessage' - The status message, if the status is @failed@.
--
-- 'suggestedAccounts', 'networkInsightsAnalysis_suggestedAccounts' - Potential intermediate accounts.
--
-- 'tags', 'networkInsightsAnalysis_tags' - The tags.
--
-- 'warningMessage', 'networkInsightsAnalysis_warningMessage' - The warning message.
newNetworkInsightsAnalysis ::
  NetworkInsightsAnalysis
newNetworkInsightsAnalysis =
  NetworkInsightsAnalysis'
    { additionalAccounts =
        Prelude.Nothing,
      alternatePathHints = Prelude.Nothing,
      explanations = Prelude.Nothing,
      filterInArns = Prelude.Nothing,
      forwardPathComponents = Prelude.Nothing,
      networkInsightsAnalysisArn = Prelude.Nothing,
      networkInsightsAnalysisId = Prelude.Nothing,
      networkInsightsPathId = Prelude.Nothing,
      networkPathFound = Prelude.Nothing,
      returnPathComponents = Prelude.Nothing,
      startDate = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      suggestedAccounts = Prelude.Nothing,
      tags = Prelude.Nothing,
      warningMessage = Prelude.Nothing
    }

-- | The member accounts that contain resources that the path can traverse.
networkInsightsAnalysis_additionalAccounts :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [Prelude.Text])
networkInsightsAnalysis_additionalAccounts = Lens.lens (\NetworkInsightsAnalysis' {additionalAccounts} -> additionalAccounts) (\s@NetworkInsightsAnalysis' {} a -> s {additionalAccounts = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | Potential intermediate components.
networkInsightsAnalysis_alternatePathHints :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [AlternatePathHint])
networkInsightsAnalysis_alternatePathHints = Lens.lens (\NetworkInsightsAnalysis' {alternatePathHints} -> alternatePathHints) (\s@NetworkInsightsAnalysis' {} a -> s {alternatePathHints = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The explanations. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
networkInsightsAnalysis_explanations :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [Explanation])
networkInsightsAnalysis_explanations = Lens.lens (\NetworkInsightsAnalysis' {explanations} -> explanations) (\s@NetworkInsightsAnalysis' {} a -> s {explanations = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARN) of the Amazon Web Services resources
-- that the path must traverse.
networkInsightsAnalysis_filterInArns :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [Prelude.Text])
networkInsightsAnalysis_filterInArns = Lens.lens (\NetworkInsightsAnalysis' {filterInArns} -> filterInArns) (\s@NetworkInsightsAnalysis' {} a -> s {filterInArns = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The components in the path from source to destination.
networkInsightsAnalysis_forwardPathComponents :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [PathComponent])
networkInsightsAnalysis_forwardPathComponents = Lens.lens (\NetworkInsightsAnalysis' {forwardPathComponents} -> forwardPathComponents) (\s@NetworkInsightsAnalysis' {} a -> s {forwardPathComponents = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the network insights analysis.
networkInsightsAnalysis_networkInsightsAnalysisArn :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAnalysis_networkInsightsAnalysisArn = Lens.lens (\NetworkInsightsAnalysis' {networkInsightsAnalysisArn} -> networkInsightsAnalysisArn) (\s@NetworkInsightsAnalysis' {} a -> s {networkInsightsAnalysisArn = a} :: NetworkInsightsAnalysis)

-- | The ID of the network insights analysis.
networkInsightsAnalysis_networkInsightsAnalysisId :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAnalysis_networkInsightsAnalysisId = Lens.lens (\NetworkInsightsAnalysis' {networkInsightsAnalysisId} -> networkInsightsAnalysisId) (\s@NetworkInsightsAnalysis' {} a -> s {networkInsightsAnalysisId = a} :: NetworkInsightsAnalysis)

-- | The ID of the path.
networkInsightsAnalysis_networkInsightsPathId :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAnalysis_networkInsightsPathId = Lens.lens (\NetworkInsightsAnalysis' {networkInsightsPathId} -> networkInsightsPathId) (\s@NetworkInsightsAnalysis' {} a -> s {networkInsightsPathId = a} :: NetworkInsightsAnalysis)

-- | Indicates whether the destination is reachable from the source.
networkInsightsAnalysis_networkPathFound :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Bool)
networkInsightsAnalysis_networkPathFound = Lens.lens (\NetworkInsightsAnalysis' {networkPathFound} -> networkPathFound) (\s@NetworkInsightsAnalysis' {} a -> s {networkPathFound = a} :: NetworkInsightsAnalysis)

-- | The components in the path from destination to source.
networkInsightsAnalysis_returnPathComponents :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [PathComponent])
networkInsightsAnalysis_returnPathComponents = Lens.lens (\NetworkInsightsAnalysis' {returnPathComponents} -> returnPathComponents) (\s@NetworkInsightsAnalysis' {} a -> s {returnPathComponents = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The time the analysis started.
networkInsightsAnalysis_startDate :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.UTCTime)
networkInsightsAnalysis_startDate = Lens.lens (\NetworkInsightsAnalysis' {startDate} -> startDate) (\s@NetworkInsightsAnalysis' {} a -> s {startDate = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Data._Time

-- | The status of the network insights analysis.
networkInsightsAnalysis_status :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe AnalysisStatus)
networkInsightsAnalysis_status = Lens.lens (\NetworkInsightsAnalysis' {status} -> status) (\s@NetworkInsightsAnalysis' {} a -> s {status = a} :: NetworkInsightsAnalysis)

-- | The status message, if the status is @failed@.
networkInsightsAnalysis_statusMessage :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAnalysis_statusMessage = Lens.lens (\NetworkInsightsAnalysis' {statusMessage} -> statusMessage) (\s@NetworkInsightsAnalysis' {} a -> s {statusMessage = a} :: NetworkInsightsAnalysis)

-- | Potential intermediate accounts.
networkInsightsAnalysis_suggestedAccounts :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [Prelude.Text])
networkInsightsAnalysis_suggestedAccounts = Lens.lens (\NetworkInsightsAnalysis' {suggestedAccounts} -> suggestedAccounts) (\s@NetworkInsightsAnalysis' {} a -> s {suggestedAccounts = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The tags.
networkInsightsAnalysis_tags :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [Tag])
networkInsightsAnalysis_tags = Lens.lens (\NetworkInsightsAnalysis' {tags} -> tags) (\s@NetworkInsightsAnalysis' {} a -> s {tags = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The warning message.
networkInsightsAnalysis_warningMessage :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAnalysis_warningMessage = Lens.lens (\NetworkInsightsAnalysis' {warningMessage} -> warningMessage) (\s@NetworkInsightsAnalysis' {} a -> s {warningMessage = a} :: NetworkInsightsAnalysis)

instance Data.FromXML NetworkInsightsAnalysis where
  parseXML x =
    NetworkInsightsAnalysis'
      Prelude.<$> ( x Data..@? "additionalAccountSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "alternatePathHintSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "explanationSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "filterInArnSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "forwardPathComponentSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "networkInsightsAnalysisArn")
      Prelude.<*> (x Data..@? "networkInsightsAnalysisId")
      Prelude.<*> (x Data..@? "networkInsightsPathId")
      Prelude.<*> (x Data..@? "networkPathFound")
      Prelude.<*> ( x Data..@? "returnPathComponentSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "startDate")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "statusMessage")
      Prelude.<*> ( x Data..@? "suggestedAccountSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "warningMessage")

instance Prelude.Hashable NetworkInsightsAnalysis where
  hashWithSalt _salt NetworkInsightsAnalysis' {..} =
    _salt `Prelude.hashWithSalt` additionalAccounts
      `Prelude.hashWithSalt` alternatePathHints
      `Prelude.hashWithSalt` explanations
      `Prelude.hashWithSalt` filterInArns
      `Prelude.hashWithSalt` forwardPathComponents
      `Prelude.hashWithSalt` networkInsightsAnalysisArn
      `Prelude.hashWithSalt` networkInsightsAnalysisId
      `Prelude.hashWithSalt` networkInsightsPathId
      `Prelude.hashWithSalt` networkPathFound
      `Prelude.hashWithSalt` returnPathComponents
      `Prelude.hashWithSalt` startDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` suggestedAccounts
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` warningMessage

instance Prelude.NFData NetworkInsightsAnalysis where
  rnf NetworkInsightsAnalysis' {..} =
    Prelude.rnf additionalAccounts
      `Prelude.seq` Prelude.rnf alternatePathHints
      `Prelude.seq` Prelude.rnf explanations
      `Prelude.seq` Prelude.rnf filterInArns
      `Prelude.seq` Prelude.rnf forwardPathComponents
      `Prelude.seq` Prelude.rnf networkInsightsAnalysisArn
      `Prelude.seq` Prelude.rnf networkInsightsAnalysisId
      `Prelude.seq` Prelude.rnf networkInsightsPathId
      `Prelude.seq` Prelude.rnf networkPathFound
      `Prelude.seq` Prelude.rnf returnPathComponents
      `Prelude.seq` Prelude.rnf startDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf suggestedAccounts
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf warningMessage
