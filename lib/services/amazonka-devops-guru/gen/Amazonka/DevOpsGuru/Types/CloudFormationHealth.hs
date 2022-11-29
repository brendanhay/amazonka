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
-- Module      : Amazonka.DevOpsGuru.Types.CloudFormationHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CloudFormationHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.InsightHealth
import qualified Amazonka.Prelude as Prelude

-- | Information about the health of Amazon Web Services resources in your
-- account that are specified by an Amazon Web Services CloudFormation
-- stack.
--
-- /See:/ 'newCloudFormationHealth' smart constructor.
data CloudFormationHealth = CloudFormationHealth'
  { -- | Information about the health of the Amazon Web Services resources in
    -- your account that are specified by an Amazon Web Services CloudFormation
    -- stack, including the number of open proactive, open reactive insights,
    -- and the Mean Time to Recover (MTTR) of closed insights.
    insight :: Prelude.Maybe InsightHealth,
    -- | Number of resources that DevOps Guru is monitoring in your account that
    -- are specified by an Amazon Web Services CloudFormation stack.
    analyzedResourceCount :: Prelude.Maybe Prelude.Integer,
    -- | The name of the CloudFormation stack.
    stackName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudFormationHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insight', 'cloudFormationHealth_insight' - Information about the health of the Amazon Web Services resources in
-- your account that are specified by an Amazon Web Services CloudFormation
-- stack, including the number of open proactive, open reactive insights,
-- and the Mean Time to Recover (MTTR) of closed insights.
--
-- 'analyzedResourceCount', 'cloudFormationHealth_analyzedResourceCount' - Number of resources that DevOps Guru is monitoring in your account that
-- are specified by an Amazon Web Services CloudFormation stack.
--
-- 'stackName', 'cloudFormationHealth_stackName' - The name of the CloudFormation stack.
newCloudFormationHealth ::
  CloudFormationHealth
newCloudFormationHealth =
  CloudFormationHealth'
    { insight = Prelude.Nothing,
      analyzedResourceCount = Prelude.Nothing,
      stackName = Prelude.Nothing
    }

-- | Information about the health of the Amazon Web Services resources in
-- your account that are specified by an Amazon Web Services CloudFormation
-- stack, including the number of open proactive, open reactive insights,
-- and the Mean Time to Recover (MTTR) of closed insights.
cloudFormationHealth_insight :: Lens.Lens' CloudFormationHealth (Prelude.Maybe InsightHealth)
cloudFormationHealth_insight = Lens.lens (\CloudFormationHealth' {insight} -> insight) (\s@CloudFormationHealth' {} a -> s {insight = a} :: CloudFormationHealth)

-- | Number of resources that DevOps Guru is monitoring in your account that
-- are specified by an Amazon Web Services CloudFormation stack.
cloudFormationHealth_analyzedResourceCount :: Lens.Lens' CloudFormationHealth (Prelude.Maybe Prelude.Integer)
cloudFormationHealth_analyzedResourceCount = Lens.lens (\CloudFormationHealth' {analyzedResourceCount} -> analyzedResourceCount) (\s@CloudFormationHealth' {} a -> s {analyzedResourceCount = a} :: CloudFormationHealth)

-- | The name of the CloudFormation stack.
cloudFormationHealth_stackName :: Lens.Lens' CloudFormationHealth (Prelude.Maybe Prelude.Text)
cloudFormationHealth_stackName = Lens.lens (\CloudFormationHealth' {stackName} -> stackName) (\s@CloudFormationHealth' {} a -> s {stackName = a} :: CloudFormationHealth)

instance Core.FromJSON CloudFormationHealth where
  parseJSON =
    Core.withObject
      "CloudFormationHealth"
      ( \x ->
          CloudFormationHealth'
            Prelude.<$> (x Core..:? "Insight")
            Prelude.<*> (x Core..:? "AnalyzedResourceCount")
            Prelude.<*> (x Core..:? "StackName")
      )

instance Prelude.Hashable CloudFormationHealth where
  hashWithSalt _salt CloudFormationHealth' {..} =
    _salt `Prelude.hashWithSalt` insight
      `Prelude.hashWithSalt` analyzedResourceCount
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData CloudFormationHealth where
  rnf CloudFormationHealth' {..} =
    Prelude.rnf insight
      `Prelude.seq` Prelude.rnf analyzedResourceCount
      `Prelude.seq` Prelude.rnf stackName
