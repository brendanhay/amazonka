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
-- Module      : Network.AWS.EC2.Types.NetworkInsightsAnalysis
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInsightsAnalysis where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.AlternatePathHint
import Network.AWS.EC2.Types.AnalysisStatus
import Network.AWS.EC2.Types.Explanation
import Network.AWS.EC2.Types.PathComponent
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a network insights analysis.
--
-- /See:/ 'newNetworkInsightsAnalysis' smart constructor.
data NetworkInsightsAnalysis = NetworkInsightsAnalysis'
  { -- | The status of the network insights analysis.
    status :: Prelude.Maybe AnalysisStatus,
    -- | The components in the path from source to destination.
    forwardPathComponents :: Prelude.Maybe [PathComponent],
    -- | Potential intermediate components.
    alternatePathHints :: Prelude.Maybe [AlternatePathHint],
    -- | The explanations. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
    explanations :: Prelude.Maybe [Explanation],
    -- | The components in the path from destination to source.
    returnPathComponents :: Prelude.Maybe [PathComponent],
    -- | The ID of the path.
    networkInsightsPathId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the Amazon Web Services resources
    -- that the path must traverse.
    filterInArns :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the network insights analysis.
    networkInsightsAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The time the analysis started.
    startDate :: Prelude.Maybe Core.ISO8601,
    -- | The Amazon Resource Name (ARN) of the network insights analysis.
    networkInsightsAnalysisArn :: Prelude.Maybe Prelude.Text,
    -- | The status message, if the status is @failed@.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the destination is reachable from the source.
    networkPathFound :: Prelude.Maybe Prelude.Bool,
    -- | The tags.
    tags :: Prelude.Maybe [Tag]
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
-- 'status', 'networkInsightsAnalysis_status' - The status of the network insights analysis.
--
-- 'forwardPathComponents', 'networkInsightsAnalysis_forwardPathComponents' - The components in the path from source to destination.
--
-- 'alternatePathHints', 'networkInsightsAnalysis_alternatePathHints' - Potential intermediate components.
--
-- 'explanations', 'networkInsightsAnalysis_explanations' - The explanations. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
--
-- 'returnPathComponents', 'networkInsightsAnalysis_returnPathComponents' - The components in the path from destination to source.
--
-- 'networkInsightsPathId', 'networkInsightsAnalysis_networkInsightsPathId' - The ID of the path.
--
-- 'filterInArns', 'networkInsightsAnalysis_filterInArns' - The Amazon Resource Names (ARN) of the Amazon Web Services resources
-- that the path must traverse.
--
-- 'networkInsightsAnalysisId', 'networkInsightsAnalysis_networkInsightsAnalysisId' - The ID of the network insights analysis.
--
-- 'startDate', 'networkInsightsAnalysis_startDate' - The time the analysis started.
--
-- 'networkInsightsAnalysisArn', 'networkInsightsAnalysis_networkInsightsAnalysisArn' - The Amazon Resource Name (ARN) of the network insights analysis.
--
-- 'statusMessage', 'networkInsightsAnalysis_statusMessage' - The status message, if the status is @failed@.
--
-- 'networkPathFound', 'networkInsightsAnalysis_networkPathFound' - Indicates whether the destination is reachable from the source.
--
-- 'tags', 'networkInsightsAnalysis_tags' - The tags.
newNetworkInsightsAnalysis ::
  NetworkInsightsAnalysis
newNetworkInsightsAnalysis =
  NetworkInsightsAnalysis'
    { status = Prelude.Nothing,
      forwardPathComponents = Prelude.Nothing,
      alternatePathHints = Prelude.Nothing,
      explanations = Prelude.Nothing,
      returnPathComponents = Prelude.Nothing,
      networkInsightsPathId = Prelude.Nothing,
      filterInArns = Prelude.Nothing,
      networkInsightsAnalysisId = Prelude.Nothing,
      startDate = Prelude.Nothing,
      networkInsightsAnalysisArn = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      networkPathFound = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The status of the network insights analysis.
networkInsightsAnalysis_status :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe AnalysisStatus)
networkInsightsAnalysis_status = Lens.lens (\NetworkInsightsAnalysis' {status} -> status) (\s@NetworkInsightsAnalysis' {} a -> s {status = a} :: NetworkInsightsAnalysis)

-- | The components in the path from source to destination.
networkInsightsAnalysis_forwardPathComponents :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [PathComponent])
networkInsightsAnalysis_forwardPathComponents = Lens.lens (\NetworkInsightsAnalysis' {forwardPathComponents} -> forwardPathComponents) (\s@NetworkInsightsAnalysis' {} a -> s {forwardPathComponents = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | Potential intermediate components.
networkInsightsAnalysis_alternatePathHints :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [AlternatePathHint])
networkInsightsAnalysis_alternatePathHints = Lens.lens (\NetworkInsightsAnalysis' {alternatePathHints} -> alternatePathHints) (\s@NetworkInsightsAnalysis' {} a -> s {alternatePathHints = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The explanations. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
networkInsightsAnalysis_explanations :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [Explanation])
networkInsightsAnalysis_explanations = Lens.lens (\NetworkInsightsAnalysis' {explanations} -> explanations) (\s@NetworkInsightsAnalysis' {} a -> s {explanations = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The components in the path from destination to source.
networkInsightsAnalysis_returnPathComponents :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [PathComponent])
networkInsightsAnalysis_returnPathComponents = Lens.lens (\NetworkInsightsAnalysis' {returnPathComponents} -> returnPathComponents) (\s@NetworkInsightsAnalysis' {} a -> s {returnPathComponents = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the path.
networkInsightsAnalysis_networkInsightsPathId :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAnalysis_networkInsightsPathId = Lens.lens (\NetworkInsightsAnalysis' {networkInsightsPathId} -> networkInsightsPathId) (\s@NetworkInsightsAnalysis' {} a -> s {networkInsightsPathId = a} :: NetworkInsightsAnalysis)

-- | The Amazon Resource Names (ARN) of the Amazon Web Services resources
-- that the path must traverse.
networkInsightsAnalysis_filterInArns :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [Prelude.Text])
networkInsightsAnalysis_filterInArns = Lens.lens (\NetworkInsightsAnalysis' {filterInArns} -> filterInArns) (\s@NetworkInsightsAnalysis' {} a -> s {filterInArns = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network insights analysis.
networkInsightsAnalysis_networkInsightsAnalysisId :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAnalysis_networkInsightsAnalysisId = Lens.lens (\NetworkInsightsAnalysis' {networkInsightsAnalysisId} -> networkInsightsAnalysisId) (\s@NetworkInsightsAnalysis' {} a -> s {networkInsightsAnalysisId = a} :: NetworkInsightsAnalysis)

-- | The time the analysis started.
networkInsightsAnalysis_startDate :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.UTCTime)
networkInsightsAnalysis_startDate = Lens.lens (\NetworkInsightsAnalysis' {startDate} -> startDate) (\s@NetworkInsightsAnalysis' {} a -> s {startDate = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the network insights analysis.
networkInsightsAnalysis_networkInsightsAnalysisArn :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAnalysis_networkInsightsAnalysisArn = Lens.lens (\NetworkInsightsAnalysis' {networkInsightsAnalysisArn} -> networkInsightsAnalysisArn) (\s@NetworkInsightsAnalysis' {} a -> s {networkInsightsAnalysisArn = a} :: NetworkInsightsAnalysis)

-- | The status message, if the status is @failed@.
networkInsightsAnalysis_statusMessage :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Text)
networkInsightsAnalysis_statusMessage = Lens.lens (\NetworkInsightsAnalysis' {statusMessage} -> statusMessage) (\s@NetworkInsightsAnalysis' {} a -> s {statusMessage = a} :: NetworkInsightsAnalysis)

-- | Indicates whether the destination is reachable from the source.
networkInsightsAnalysis_networkPathFound :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe Prelude.Bool)
networkInsightsAnalysis_networkPathFound = Lens.lens (\NetworkInsightsAnalysis' {networkPathFound} -> networkPathFound) (\s@NetworkInsightsAnalysis' {} a -> s {networkPathFound = a} :: NetworkInsightsAnalysis)

-- | The tags.
networkInsightsAnalysis_tags :: Lens.Lens' NetworkInsightsAnalysis (Prelude.Maybe [Tag])
networkInsightsAnalysis_tags = Lens.lens (\NetworkInsightsAnalysis' {tags} -> tags) (\s@NetworkInsightsAnalysis' {} a -> s {tags = a} :: NetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML NetworkInsightsAnalysis where
  parseXML x =
    NetworkInsightsAnalysis'
      Prelude.<$> (x Core..@? "status")
      Prelude.<*> ( x Core..@? "forwardPathComponentSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "alternatePathHintSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "explanationSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "returnPathComponentSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "networkInsightsPathId")
      Prelude.<*> ( x Core..@? "filterInArnSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "networkInsightsAnalysisId")
      Prelude.<*> (x Core..@? "startDate")
      Prelude.<*> (x Core..@? "networkInsightsAnalysisArn")
      Prelude.<*> (x Core..@? "statusMessage")
      Prelude.<*> (x Core..@? "networkPathFound")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable NetworkInsightsAnalysis

instance Prelude.NFData NetworkInsightsAnalysis
