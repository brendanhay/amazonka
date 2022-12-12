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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a profiling group.
--
-- /See:/ 'newProfilingGroupDescription' smart constructor.
data ProfilingGroupDescription = ProfilingGroupDescription'
  { -- | An
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AgentOrchestrationConfig.html AgentOrchestrationConfig>
    -- object that indicates if the profiling group is enabled for profiled or
    -- not.
    agentOrchestrationConfig :: Prelude.Maybe AgentOrchestrationConfig,
    -- | The Amazon Resource Name (ARN) identifying the profiling group resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The compute platform of the profiling group. If it is set to
    -- @AWSLambda@, then the profiled application runs on AWS Lambda. If it is
    -- set to @Default@, then the profiled application runs on a compute
    -- platform that is not AWS Lambda, such an Amazon EC2 instance, an
    -- on-premises server, or a different platform. The default is @Default@.
    computePlatform :: Prelude.Maybe ComputePlatform,
    -- | The time when the profiling group was created. Specify using the ISO
    -- 8601 format. For example, 2020-06-01T13:15:02.001Z represents 1
    -- millisecond past June 1, 2020 1:15:02 PM UTC.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the profiling group.
    name :: Prelude.Maybe Prelude.Text,
    -- | A
    -- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingStatus.html ProfilingStatus>
    -- object that includes information about the last time a profile agent
    -- pinged back, the last time a profile was received, and the aggregation
    -- period and start time for the most recent aggregated profile.
    profilingStatus :: Prelude.Maybe ProfilingStatus,
    -- | A list of the tags that belong to this profiling group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time when the profiling group was last updated. Specify
    -- using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
    -- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
    updatedAt :: Prelude.Maybe Data.POSIX
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
-- 'agentOrchestrationConfig', 'profilingGroupDescription_agentOrchestrationConfig' - An
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AgentOrchestrationConfig.html AgentOrchestrationConfig>
-- object that indicates if the profiling group is enabled for profiled or
-- not.
--
-- 'arn', 'profilingGroupDescription_arn' - The Amazon Resource Name (ARN) identifying the profiling group resource.
--
-- 'computePlatform', 'profilingGroupDescription_computePlatform' - The compute platform of the profiling group. If it is set to
-- @AWSLambda@, then the profiled application runs on AWS Lambda. If it is
-- set to @Default@, then the profiled application runs on a compute
-- platform that is not AWS Lambda, such an Amazon EC2 instance, an
-- on-premises server, or a different platform. The default is @Default@.
--
-- 'createdAt', 'profilingGroupDescription_createdAt' - The time when the profiling group was created. Specify using the ISO
-- 8601 format. For example, 2020-06-01T13:15:02.001Z represents 1
-- millisecond past June 1, 2020 1:15:02 PM UTC.
--
-- 'name', 'profilingGroupDescription_name' - The name of the profiling group.
--
-- 'profilingStatus', 'profilingGroupDescription_profilingStatus' - A
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingStatus.html ProfilingStatus>
-- object that includes information about the last time a profile agent
-- pinged back, the last time a profile was received, and the aggregation
-- period and start time for the most recent aggregated profile.
--
-- 'tags', 'profilingGroupDescription_tags' - A list of the tags that belong to this profiling group.
--
-- 'updatedAt', 'profilingGroupDescription_updatedAt' - The date and time when the profiling group was last updated. Specify
-- using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
-- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
newProfilingGroupDescription ::
  ProfilingGroupDescription
newProfilingGroupDescription =
  ProfilingGroupDescription'
    { agentOrchestrationConfig =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      computePlatform = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      profilingStatus = Prelude.Nothing,
      tags = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | An
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_AgentOrchestrationConfig.html AgentOrchestrationConfig>
-- object that indicates if the profiling group is enabled for profiled or
-- not.
profilingGroupDescription_agentOrchestrationConfig :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe AgentOrchestrationConfig)
profilingGroupDescription_agentOrchestrationConfig = Lens.lens (\ProfilingGroupDescription' {agentOrchestrationConfig} -> agentOrchestrationConfig) (\s@ProfilingGroupDescription' {} a -> s {agentOrchestrationConfig = a} :: ProfilingGroupDescription)

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

-- | The time when the profiling group was created. Specify using the ISO
-- 8601 format. For example, 2020-06-01T13:15:02.001Z represents 1
-- millisecond past June 1, 2020 1:15:02 PM UTC.
profilingGroupDescription_createdAt :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe Prelude.UTCTime)
profilingGroupDescription_createdAt = Lens.lens (\ProfilingGroupDescription' {createdAt} -> createdAt) (\s@ProfilingGroupDescription' {} a -> s {createdAt = a} :: ProfilingGroupDescription) Prelude.. Lens.mapping Data._Time

-- | The name of the profiling group.
profilingGroupDescription_name :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe Prelude.Text)
profilingGroupDescription_name = Lens.lens (\ProfilingGroupDescription' {name} -> name) (\s@ProfilingGroupDescription' {} a -> s {name = a} :: ProfilingGroupDescription)

-- | A
-- <https://docs.aws.amazon.com/codeguru/latest/profiler-api/API_ProfilingStatus.html ProfilingStatus>
-- object that includes information about the last time a profile agent
-- pinged back, the last time a profile was received, and the aggregation
-- period and start time for the most recent aggregated profile.
profilingGroupDescription_profilingStatus :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe ProfilingStatus)
profilingGroupDescription_profilingStatus = Lens.lens (\ProfilingGroupDescription' {profilingStatus} -> profilingStatus) (\s@ProfilingGroupDescription' {} a -> s {profilingStatus = a} :: ProfilingGroupDescription)

-- | A list of the tags that belong to this profiling group.
profilingGroupDescription_tags :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
profilingGroupDescription_tags = Lens.lens (\ProfilingGroupDescription' {tags} -> tags) (\s@ProfilingGroupDescription' {} a -> s {tags = a} :: ProfilingGroupDescription) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the profiling group was last updated. Specify
-- using the ISO 8601 format. For example, 2020-06-01T13:15:02.001Z
-- represents 1 millisecond past June 1, 2020 1:15:02 PM UTC.
profilingGroupDescription_updatedAt :: Lens.Lens' ProfilingGroupDescription (Prelude.Maybe Prelude.UTCTime)
profilingGroupDescription_updatedAt = Lens.lens (\ProfilingGroupDescription' {updatedAt} -> updatedAt) (\s@ProfilingGroupDescription' {} a -> s {updatedAt = a} :: ProfilingGroupDescription) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ProfilingGroupDescription where
  parseJSON =
    Data.withObject
      "ProfilingGroupDescription"
      ( \x ->
          ProfilingGroupDescription'
            Prelude.<$> (x Data..:? "agentOrchestrationConfig")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "computePlatform")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "profilingStatus")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable ProfilingGroupDescription where
  hashWithSalt _salt ProfilingGroupDescription' {..} =
    _salt
      `Prelude.hashWithSalt` agentOrchestrationConfig
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` computePlatform
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` profilingStatus
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData ProfilingGroupDescription where
  rnf ProfilingGroupDescription' {..} =
    Prelude.rnf agentOrchestrationConfig
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf computePlatform
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf profilingStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updatedAt
