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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.ResourceResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.ResourceResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryReadiness.Types.Readiness

-- | The result of a successful Resource request, with status for an
-- individual resource.
--
-- /See:/ 'newResourceResult' smart constructor.
data ResourceResult = ResourceResult'
  { -- | The Amazon Resource Name (ARN) of the resource.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The component id of the resource.
    componentId :: Prelude.Maybe Prelude.Text,
    -- | The readiness of a resource.
    readiness :: Readiness,
    -- | The time (UTC) that the resource was last checked for readiness, in
    -- ISO-8601 format.
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
-- 'resourceArn', 'resourceResult_resourceArn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'componentId', 'resourceResult_componentId' - The component id of the resource.
--
-- 'readiness', 'resourceResult_readiness' - The readiness of a resource.
--
-- 'lastCheckedTimestamp', 'resourceResult_lastCheckedTimestamp' - The time (UTC) that the resource was last checked for readiness, in
-- ISO-8601 format.
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

-- | The Amazon Resource Name (ARN) of the resource.
resourceResult_resourceArn :: Lens.Lens' ResourceResult (Prelude.Maybe Prelude.Text)
resourceResult_resourceArn = Lens.lens (\ResourceResult' {resourceArn} -> resourceArn) (\s@ResourceResult' {} a -> s {resourceArn = a} :: ResourceResult)

-- | The component id of the resource.
resourceResult_componentId :: Lens.Lens' ResourceResult (Prelude.Maybe Prelude.Text)
resourceResult_componentId = Lens.lens (\ResourceResult' {componentId} -> componentId) (\s@ResourceResult' {} a -> s {componentId = a} :: ResourceResult)

-- | The readiness of a resource.
resourceResult_readiness :: Lens.Lens' ResourceResult Readiness
resourceResult_readiness = Lens.lens (\ResourceResult' {readiness} -> readiness) (\s@ResourceResult' {} a -> s {readiness = a} :: ResourceResult)

-- | The time (UTC) that the resource was last checked for readiness, in
-- ISO-8601 format.
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

instance Prelude.Hashable ResourceResult where
  hashWithSalt _salt ResourceResult' {..} =
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` readiness
      `Prelude.hashWithSalt` lastCheckedTimestamp

instance Prelude.NFData ResourceResult where
  rnf ResourceResult' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf readiness
      `Prelude.seq` Prelude.rnf lastCheckedTimestamp
