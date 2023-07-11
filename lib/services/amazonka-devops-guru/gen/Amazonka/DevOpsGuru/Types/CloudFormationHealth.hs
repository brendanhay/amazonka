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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CloudFormationHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.InsightHealth
import qualified Amazonka.Prelude as Prelude

-- | Information about the health of Amazon Web Services resources in your
-- account that are specified by an Amazon Web Services CloudFormation
-- stack.
--
-- /See:/ 'newCloudFormationHealth' smart constructor.
data CloudFormationHealth = CloudFormationHealth'
  { -- | Number of resources that DevOps Guru is monitoring in your account that
    -- are specified by an Amazon Web Services CloudFormation stack.
    analyzedResourceCount :: Prelude.Maybe Prelude.Integer,
    -- | Information about the health of the Amazon Web Services resources in
    -- your account that are specified by an Amazon Web Services CloudFormation
    -- stack, including the number of open proactive, open reactive insights,
    -- and the Mean Time to Recover (MTTR) of closed insights.
    insight :: Prelude.Maybe InsightHealth,
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
-- 'analyzedResourceCount', 'cloudFormationHealth_analyzedResourceCount' - Number of resources that DevOps Guru is monitoring in your account that
-- are specified by an Amazon Web Services CloudFormation stack.
--
-- 'insight', 'cloudFormationHealth_insight' - Information about the health of the Amazon Web Services resources in
-- your account that are specified by an Amazon Web Services CloudFormation
-- stack, including the number of open proactive, open reactive insights,
-- and the Mean Time to Recover (MTTR) of closed insights.
--
-- 'stackName', 'cloudFormationHealth_stackName' - The name of the CloudFormation stack.
newCloudFormationHealth ::
  CloudFormationHealth
newCloudFormationHealth =
  CloudFormationHealth'
    { analyzedResourceCount =
        Prelude.Nothing,
      insight = Prelude.Nothing,
      stackName = Prelude.Nothing
    }

-- | Number of resources that DevOps Guru is monitoring in your account that
-- are specified by an Amazon Web Services CloudFormation stack.
cloudFormationHealth_analyzedResourceCount :: Lens.Lens' CloudFormationHealth (Prelude.Maybe Prelude.Integer)
cloudFormationHealth_analyzedResourceCount = Lens.lens (\CloudFormationHealth' {analyzedResourceCount} -> analyzedResourceCount) (\s@CloudFormationHealth' {} a -> s {analyzedResourceCount = a} :: CloudFormationHealth)

-- | Information about the health of the Amazon Web Services resources in
-- your account that are specified by an Amazon Web Services CloudFormation
-- stack, including the number of open proactive, open reactive insights,
-- and the Mean Time to Recover (MTTR) of closed insights.
cloudFormationHealth_insight :: Lens.Lens' CloudFormationHealth (Prelude.Maybe InsightHealth)
cloudFormationHealth_insight = Lens.lens (\CloudFormationHealth' {insight} -> insight) (\s@CloudFormationHealth' {} a -> s {insight = a} :: CloudFormationHealth)

-- | The name of the CloudFormation stack.
cloudFormationHealth_stackName :: Lens.Lens' CloudFormationHealth (Prelude.Maybe Prelude.Text)
cloudFormationHealth_stackName = Lens.lens (\CloudFormationHealth' {stackName} -> stackName) (\s@CloudFormationHealth' {} a -> s {stackName = a} :: CloudFormationHealth)

instance Data.FromJSON CloudFormationHealth where
  parseJSON =
    Data.withObject
      "CloudFormationHealth"
      ( \x ->
          CloudFormationHealth'
            Prelude.<$> (x Data..:? "AnalyzedResourceCount")
            Prelude.<*> (x Data..:? "Insight")
            Prelude.<*> (x Data..:? "StackName")
      )

instance Prelude.Hashable CloudFormationHealth where
  hashWithSalt _salt CloudFormationHealth' {..} =
    _salt
      `Prelude.hashWithSalt` analyzedResourceCount
      `Prelude.hashWithSalt` insight
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData CloudFormationHealth where
  rnf CloudFormationHealth' {..} =
    Prelude.rnf analyzedResourceCount
      `Prelude.seq` Prelude.rnf insight
      `Prelude.seq` Prelude.rnf stackName
