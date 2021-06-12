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

-- | Describes a network insights analysis.
--
-- /See:/ 'newNetworkInsightsAnalysis' smart constructor.
data NetworkInsightsAnalysis = NetworkInsightsAnalysis'
  { -- | The status message, if the status is @failed@.
    statusMessage :: Core.Maybe Core.Text,
    -- | The status of the network insights analysis.
    status :: Core.Maybe AnalysisStatus,
    -- | The ID of the network insights analysis.
    networkInsightsAnalysisId :: Core.Maybe Core.Text,
    -- | The time the analysis started.
    startDate :: Core.Maybe Core.ISO8601,
    -- | The Amazon Resource Names (ARN) of the AWS resources that the path must
    -- traverse.
    filterInArns :: Core.Maybe [Core.Text],
    -- | The components in the path from destination to source.
    returnPathComponents :: Core.Maybe [PathComponent],
    -- | The explanations. For more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
    explanations :: Core.Maybe [Explanation],
    -- | Indicates whether the destination is reachable from the source.
    networkPathFound :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the network insights analysis.
    networkInsightsAnalysisArn :: Core.Maybe Core.Text,
    -- | The tags.
    tags :: Core.Maybe [Tag],
    -- | The ID of the path.
    networkInsightsPathId :: Core.Maybe Core.Text,
    -- | Potential intermediate components.
    alternatePathHints :: Core.Maybe [AlternatePathHint],
    -- | The components in the path from source to destination.
    forwardPathComponents :: Core.Maybe [PathComponent]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NetworkInsightsAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'networkInsightsAnalysis_statusMessage' - The status message, if the status is @failed@.
--
-- 'status', 'networkInsightsAnalysis_status' - The status of the network insights analysis.
--
-- 'networkInsightsAnalysisId', 'networkInsightsAnalysis_networkInsightsAnalysisId' - The ID of the network insights analysis.
--
-- 'startDate', 'networkInsightsAnalysis_startDate' - The time the analysis started.
--
-- 'filterInArns', 'networkInsightsAnalysis_filterInArns' - The Amazon Resource Names (ARN) of the AWS resources that the path must
-- traverse.
--
-- 'returnPathComponents', 'networkInsightsAnalysis_returnPathComponents' - The components in the path from destination to source.
--
-- 'explanations', 'networkInsightsAnalysis_explanations' - The explanations. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
--
-- 'networkPathFound', 'networkInsightsAnalysis_networkPathFound' - Indicates whether the destination is reachable from the source.
--
-- 'networkInsightsAnalysisArn', 'networkInsightsAnalysis_networkInsightsAnalysisArn' - The Amazon Resource Name (ARN) of the network insights analysis.
--
-- 'tags', 'networkInsightsAnalysis_tags' - The tags.
--
-- 'networkInsightsPathId', 'networkInsightsAnalysis_networkInsightsPathId' - The ID of the path.
--
-- 'alternatePathHints', 'networkInsightsAnalysis_alternatePathHints' - Potential intermediate components.
--
-- 'forwardPathComponents', 'networkInsightsAnalysis_forwardPathComponents' - The components in the path from source to destination.
newNetworkInsightsAnalysis ::
  NetworkInsightsAnalysis
newNetworkInsightsAnalysis =
  NetworkInsightsAnalysis'
    { statusMessage =
        Core.Nothing,
      status = Core.Nothing,
      networkInsightsAnalysisId = Core.Nothing,
      startDate = Core.Nothing,
      filterInArns = Core.Nothing,
      returnPathComponents = Core.Nothing,
      explanations = Core.Nothing,
      networkPathFound = Core.Nothing,
      networkInsightsAnalysisArn = Core.Nothing,
      tags = Core.Nothing,
      networkInsightsPathId = Core.Nothing,
      alternatePathHints = Core.Nothing,
      forwardPathComponents = Core.Nothing
    }

-- | The status message, if the status is @failed@.
networkInsightsAnalysis_statusMessage :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe Core.Text)
networkInsightsAnalysis_statusMessage = Lens.lens (\NetworkInsightsAnalysis' {statusMessage} -> statusMessage) (\s@NetworkInsightsAnalysis' {} a -> s {statusMessage = a} :: NetworkInsightsAnalysis)

-- | The status of the network insights analysis.
networkInsightsAnalysis_status :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe AnalysisStatus)
networkInsightsAnalysis_status = Lens.lens (\NetworkInsightsAnalysis' {status} -> status) (\s@NetworkInsightsAnalysis' {} a -> s {status = a} :: NetworkInsightsAnalysis)

-- | The ID of the network insights analysis.
networkInsightsAnalysis_networkInsightsAnalysisId :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe Core.Text)
networkInsightsAnalysis_networkInsightsAnalysisId = Lens.lens (\NetworkInsightsAnalysis' {networkInsightsAnalysisId} -> networkInsightsAnalysisId) (\s@NetworkInsightsAnalysis' {} a -> s {networkInsightsAnalysisId = a} :: NetworkInsightsAnalysis)

-- | The time the analysis started.
networkInsightsAnalysis_startDate :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe Core.UTCTime)
networkInsightsAnalysis_startDate = Lens.lens (\NetworkInsightsAnalysis' {startDate} -> startDate) (\s@NetworkInsightsAnalysis' {} a -> s {startDate = a} :: NetworkInsightsAnalysis) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Names (ARN) of the AWS resources that the path must
-- traverse.
networkInsightsAnalysis_filterInArns :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe [Core.Text])
networkInsightsAnalysis_filterInArns = Lens.lens (\NetworkInsightsAnalysis' {filterInArns} -> filterInArns) (\s@NetworkInsightsAnalysis' {} a -> s {filterInArns = a} :: NetworkInsightsAnalysis) Core.. Lens.mapping Lens._Coerce

-- | The components in the path from destination to source.
networkInsightsAnalysis_returnPathComponents :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe [PathComponent])
networkInsightsAnalysis_returnPathComponents = Lens.lens (\NetworkInsightsAnalysis' {returnPathComponents} -> returnPathComponents) (\s@NetworkInsightsAnalysis' {} a -> s {returnPathComponents = a} :: NetworkInsightsAnalysis) Core.. Lens.mapping Lens._Coerce

-- | The explanations. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/reachability/explanation-codes.html Reachability Analyzer explanation codes>.
networkInsightsAnalysis_explanations :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe [Explanation])
networkInsightsAnalysis_explanations = Lens.lens (\NetworkInsightsAnalysis' {explanations} -> explanations) (\s@NetworkInsightsAnalysis' {} a -> s {explanations = a} :: NetworkInsightsAnalysis) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the destination is reachable from the source.
networkInsightsAnalysis_networkPathFound :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe Core.Bool)
networkInsightsAnalysis_networkPathFound = Lens.lens (\NetworkInsightsAnalysis' {networkPathFound} -> networkPathFound) (\s@NetworkInsightsAnalysis' {} a -> s {networkPathFound = a} :: NetworkInsightsAnalysis)

-- | The Amazon Resource Name (ARN) of the network insights analysis.
networkInsightsAnalysis_networkInsightsAnalysisArn :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe Core.Text)
networkInsightsAnalysis_networkInsightsAnalysisArn = Lens.lens (\NetworkInsightsAnalysis' {networkInsightsAnalysisArn} -> networkInsightsAnalysisArn) (\s@NetworkInsightsAnalysis' {} a -> s {networkInsightsAnalysisArn = a} :: NetworkInsightsAnalysis)

-- | The tags.
networkInsightsAnalysis_tags :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe [Tag])
networkInsightsAnalysis_tags = Lens.lens (\NetworkInsightsAnalysis' {tags} -> tags) (\s@NetworkInsightsAnalysis' {} a -> s {tags = a} :: NetworkInsightsAnalysis) Core.. Lens.mapping Lens._Coerce

-- | The ID of the path.
networkInsightsAnalysis_networkInsightsPathId :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe Core.Text)
networkInsightsAnalysis_networkInsightsPathId = Lens.lens (\NetworkInsightsAnalysis' {networkInsightsPathId} -> networkInsightsPathId) (\s@NetworkInsightsAnalysis' {} a -> s {networkInsightsPathId = a} :: NetworkInsightsAnalysis)

-- | Potential intermediate components.
networkInsightsAnalysis_alternatePathHints :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe [AlternatePathHint])
networkInsightsAnalysis_alternatePathHints = Lens.lens (\NetworkInsightsAnalysis' {alternatePathHints} -> alternatePathHints) (\s@NetworkInsightsAnalysis' {} a -> s {alternatePathHints = a} :: NetworkInsightsAnalysis) Core.. Lens.mapping Lens._Coerce

-- | The components in the path from source to destination.
networkInsightsAnalysis_forwardPathComponents :: Lens.Lens' NetworkInsightsAnalysis (Core.Maybe [PathComponent])
networkInsightsAnalysis_forwardPathComponents = Lens.lens (\NetworkInsightsAnalysis' {forwardPathComponents} -> forwardPathComponents) (\s@NetworkInsightsAnalysis' {} a -> s {forwardPathComponents = a} :: NetworkInsightsAnalysis) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML NetworkInsightsAnalysis where
  parseXML x =
    NetworkInsightsAnalysis'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "networkInsightsAnalysisId")
      Core.<*> (x Core..@? "startDate")
      Core.<*> ( x Core..@? "filterInArnSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "returnPathComponentSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "explanationSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "networkPathFound")
      Core.<*> (x Core..@? "networkInsightsAnalysisArn")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "networkInsightsPathId")
      Core.<*> ( x Core..@? "alternatePathHintSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "forwardPathComponentSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable NetworkInsightsAnalysis

instance Core.NFData NetworkInsightsAnalysis
