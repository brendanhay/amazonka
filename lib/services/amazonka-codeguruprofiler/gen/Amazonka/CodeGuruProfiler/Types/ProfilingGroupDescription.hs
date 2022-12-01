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
-- Module      : Amazonka.CodeGuruProfiler.Types.ProfilingGroupDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.ProfilingGroupDescription where

import Amazonka.CodeGuruProfiler.Types.AgentOrchestrationConfig
import Amazonka.CodeGuruProfiler.Types.ComputePlatform
import Amazonka.CodeGuruProfiler.Types.ProfilingStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a profiling group.
--
-- /See:/ 'newProfilingGroupDescription' smart constructor.
data ProfilingGroupDescription = ProfilingGroupDescription'
  { -- | A list of the tags that belong to this profiling group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the profiling group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) identifying the profiling group resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The compute platform of the profiling group. If it is set to
    -- @AWSLambda@, then the profiled application runs on AWS Lambda. If it is
    -- set to @Default@, then the profiled application runs on a compute
    -- platform that is not AWS Lambda, such an Amazon EC2 instance, an
    -- on-premises server, or a different platform. The default is @Default@.
    computePlatform :: Prelude.Maybe ComputePlatform,
    -- | A
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingStatus.html ProfilingStatus>
    -- object that includes information about the last time a profile agent
    -- pinged back, the last time a profile was received, and the aggregation
    -- period and start time for the most recent aggregated profile.
    profilingStatus :: Prelude.Maybe ProfilingStatus,
    -- | The time when the profiling group was created. Specify using the ISO
    -- 8601 format. For example, 2020-06-01T13:15:02.001Z represents 1
    -- millisecond past June 1, 2020 1:15:02 PM UTC.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The date and time when the profiling group was last updated. Specify
    -- using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
    -- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | An
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AgentOrchestrationConfig.html AgentOrchestrationConfig>
    -- object that indicates if the profiling group is enabled for profiled or
    -- not.
    agentOrchestrationConfig :: Prelude.Maybe AgentOrchestrationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfilingGroupDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'profilingGroupDescription_tags' - A list of the tags that belong to this profiling group.
--
-- 'name', 'profilingGroupDescription_name' - The name of the profiling group.
--
-- 'arn', 'profilingGroupDescription_arn' - The Amazon Resource Name (ARN) identifying the profiling group resource.
--
-- 'computePlatform', 'profilingGroupDescription_computePlatform' - The compute platform of the profiling group. If it is set to
-- @AWSLambda@, then the profiled application runs on AWS Lambda. If it is
-- set to @Default@, then the profiled application runs on a compute
-- platform that is not AWS Lambda, such an Amazon EC2 instance, an
-- on-premises server, or a different platform. The default is @Default@.
--
-- 'profilingStatus', 'profilingGroupDescription_profilingStatus' - A
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingStatus.html ProfilingStatus>
-- object that includes information about the last time a profile agent
-- pinged back, the last time a profile was received, and the aggregation
-- period and start time for the most recent aggregated profile.
--
-- 'createdAt', 'profilingGroupDescription_createdAt' - The time when the profiling group was created. Specify using the ISO
-- 8601 format. For example, 2020-06-01T13:15:02.001Z represents 1
-- millisecond past June 1, 2020 1:15:02 PM UTC.
--
-- 'updatedAt', 'profilingGroupDescription_updatedAt' - The date and time when the profiling group was last updated. Specify
-- using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
-- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
--
-- 'agentOrchestrationConfig', 'profilingGroupDescription_agentOrchestrationConfig' - An
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AgentOrchestrationConfig.html AgentOrchestrationConfig>
-- object that indicates if the profiling group is enabled for profiled or
-- not.
newProfilingGroupDescription ::
  ProfilingGroupDescription
newProfilingGroupDescription =
  ProfilingGroupDescription'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      computePlatform = Prelude.Nothing,
      profilingStatus = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      agentOrchestrationConfig = Prelude.Nothing
    }

-- | A list of the tags that belong to this profiling group.
profilingGroupDescription_tags :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
profilingGroupDescription_tags = Lens.lens (\ProfilingGroupDescription' {tags} -> tags) (\s@ProfilingGroupDescription' {} a -> s {tags = a} :: ProfilingGroupDescription) Prelude.. Lens.mapping Lens.coerced

-- | The name of the profiling group.
profilingGroupDescription_name :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe Prelude.Text)
profilingGroupDescription_name = Lens.lens (\ProfilingGroupDescription' {name} -> name) (\s@ProfilingGroupDescription' {} a -> s {name = a} :: ProfilingGroupDescription)

-- | The Amazon Resource Name (ARN) identifying the profiling group resource.
profilingGroupDescription_arn :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe Prelude.Text)
profilingGroupDescription_arn = Lens.lens (\ProfilingGroupDescription' {arn} -> arn) (\s@ProfilingGroupDescription' {} a -> s {arn = a} :: ProfilingGroupDescription)

-- | The compute platform of the profiling group. If it is set to
-- @AWSLambda@, then the profiled application runs on AWS Lambda. If it is
-- set to @Default@, then the profiled application runs on a compute
-- platform that is not AWS Lambda, such an Amazon EC2 instance, an
-- on-premises server, or a different platform. The default is @Default@.
profilingGroupDescription_computePlatform :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe ComputePlatform)
profilingGroupDescription_computePlatform = Lens.lens (\ProfilingGroupDescription' {computePlatform} -> computePlatform) (\s@ProfilingGroupDescription' {} a -> s {computePlatform = a} :: ProfilingGroupDescription)

-- | A
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingStatus.html ProfilingStatus>
-- object that includes information about the last time a profile agent
-- pinged back, the last time a profile was received, and the aggregation
-- period and start time for the most recent aggregated profile.
profilingGroupDescription_profilingStatus :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe ProfilingStatus)
profilingGroupDescription_profilingStatus = Lens.lens (\ProfilingGroupDescription' {profilingStatus} -> profilingStatus) (\s@ProfilingGroupDescription' {} a -> s {profilingStatus = a} :: ProfilingGroupDescription)

-- | The time when the profiling group was created. Specify using the ISO
-- 8601 format. For example, 2020-06-01T13:15:02.001Z represents 1
-- millisecond past June 1, 2020 1:15:02 PM UTC.
profilingGroupDescription_createdAt :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe Prelude.UTCTime)
profilingGroupDescription_createdAt = Lens.lens (\ProfilingGroupDescription' {createdAt} -> createdAt) (\s@ProfilingGroupDescription' {} a -> s {createdAt = a} :: ProfilingGroupDescription) Prelude.. Lens.mapping Core._Time

-- | The date and time when the profiling group was last updated. Specify
-- using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
-- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
profilingGroupDescription_updatedAt :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe Prelude.UTCTime)
profilingGroupDescription_updatedAt = Lens.lens (\ProfilingGroupDescription' {updatedAt} -> updatedAt) (\s@ProfilingGroupDescription' {} a -> s {updatedAt = a} :: ProfilingGroupDescription) Prelude.. Lens.mapping Core._Time

-- | An
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AgentOrchestrationConfig.html AgentOrchestrationConfig>
-- object that indicates if the profiling group is enabled for profiled or
-- not.
profilingGroupDescription_agentOrchestrationConfig :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe AgentOrchestrationConfig)
profilingGroupDescription_agentOrchestrationConfig = Lens.lens (\ProfilingGroupDescription' {agentOrchestrationConfig} -> agentOrchestrationConfig) (\s@ProfilingGroupDescription' {} a -> s {agentOrchestrationConfig = a} :: ProfilingGroupDescription)

instance Core.FromJSON ProfilingGroupDescription where
  parseJSON =
    Core.withObject
      "ProfilingGroupDescription"
      ( \x ->
          ProfilingGroupDescription'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "computePlatform")
            Prelude.<*> (x Core..:? "profilingStatus")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> (x Core..:? "agentOrchestrationConfig")
      )

instance Prelude.Hashable ProfilingGroupDescription where
  hashWithSalt _salt ProfilingGroupDescription' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` computePlatform
      `Prelude.hashWithSalt` profilingStatus
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` agentOrchestrationConfig

instance Prelude.NFData ProfilingGroupDescription where
  rnf ProfilingGroupDescription' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf computePlatform
      `Prelude.seq` Prelude.rnf profilingStatus
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf agentOrchestrationConfig
