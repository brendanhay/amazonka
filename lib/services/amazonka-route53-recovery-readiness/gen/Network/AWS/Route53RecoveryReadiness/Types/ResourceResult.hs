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
-- Module      : Network.AWS.Route53RecoveryReadiness.Types.ResourceResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53RecoveryReadiness.Types.ResourceResult where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53RecoveryReadiness.Types.Readiness

-- | Result with status for an individual resource.
--
-- /See:/ 'newResourceResult' smart constructor.
data ResourceResult = ResourceResult'
  { -- | The ARN of the resource
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The component id of the resource
    componentId :: Prelude.Maybe Prelude.Text,
    -- | The readiness of the resource.
    readiness :: Readiness,
    -- | The time the resource was last checked for readiness, in ISO-8601
    -- format, UTC.
    lastCheckedTimestamp :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'resourceResult_resourceArn' - The ARN of the resource
--
-- 'componentId', 'resourceResult_componentId' - The component id of the resource
--
-- 'readiness', 'resourceResult_readiness' - The readiness of the resource.
--
-- 'lastCheckedTimestamp', 'resourceResult_lastCheckedTimestamp' - The time the resource was last checked for readiness, in ISO-8601
-- format, UTC.
newResourceResult ::
  -- | 'readiness'
  Readiness ->
  -- | 'lastCheckedTimestamp'
  Prelude.UTCTime ->
  ResourceResult
newResourceResult pReadiness_ pLastCheckedTimestamp_ =
  ResourceResult'
    { resourceArn = Prelude.Nothing,
      componentId = Prelude.Nothing,
      readiness = pReadiness_,
      lastCheckedTimestamp =
        Core._Time Lens.# pLastCheckedTimestamp_
    }

-- | The ARN of the resource
resourceResult_resourceArn :: Lens.Lens' ResourceResult (Prelude.Maybe Prelude.Text)
resourceResult_resourceArn = Lens.lens (\ResourceResult' {resourceArn} -> resourceArn) (\s@ResourceResult' {} a -> s {resourceArn = a} :: ResourceResult)

-- | The component id of the resource
resourceResult_componentId :: Lens.Lens' ResourceResult (Prelude.Maybe Prelude.Text)
resourceResult_componentId = Lens.lens (\ResourceResult' {componentId} -> componentId) (\s@ResourceResult' {} a -> s {componentId = a} :: ResourceResult)

-- | The readiness of the resource.
resourceResult_readiness :: Lens.Lens' ResourceResult Readiness
resourceResult_readiness = Lens.lens (\ResourceResult' {readiness} -> readiness) (\s@ResourceResult' {} a -> s {readiness = a} :: ResourceResult)

-- | The time the resource was last checked for readiness, in ISO-8601
-- format, UTC.
resourceResult_lastCheckedTimestamp :: Lens.Lens' ResourceResult Prelude.UTCTime
resourceResult_lastCheckedTimestamp = Lens.lens (\ResourceResult' {lastCheckedTimestamp} -> lastCheckedTimestamp) (\s@ResourceResult' {} a -> s {lastCheckedTimestamp = a} :: ResourceResult) Prelude.. Core._Time

instance Core.FromJSON ResourceResult where
  parseJSON =
    Core.withObject
      "ResourceResult"
      ( \x ->
          ResourceResult'
            Prelude.<$> (x Core..:? "resourceArn")
            Prelude.<*> (x Core..:? "componentId")
            Prelude.<*> (x Core..: "readiness")
            Prelude.<*> (x Core..: "lastCheckedTimestamp")
      )

instance Prelude.Hashable ResourceResult

instance Prelude.NFData ResourceResult
