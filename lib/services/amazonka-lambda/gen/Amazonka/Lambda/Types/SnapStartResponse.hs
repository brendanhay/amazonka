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
-- Module      : Amazonka.Lambda.Types.SnapStartResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.SnapStartResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.SnapStartApplyOn
import Amazonka.Lambda.Types.SnapStartOptimizationStatus
import qualified Amazonka.Prelude as Prelude

-- | The function\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html SnapStart>
-- setting.
--
-- /See:/ 'newSnapStartResponse' smart constructor.
data SnapStartResponse = SnapStartResponse'
  { -- | When set to @PublishedVersions@, Lambda creates a snapshot of the
    -- execution environment when you publish a function version.
    applyOn :: Prelude.Maybe SnapStartApplyOn,
    -- | When you provide a
    -- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html#versioning-versions-using qualified Amazon Resource Name (ARN)>,
    -- this response element indicates whether SnapStart is activated for the
    -- specified function version.
    optimizationStatus :: Prelude.Maybe SnapStartOptimizationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapStartResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyOn', 'snapStartResponse_applyOn' - When set to @PublishedVersions@, Lambda creates a snapshot of the
-- execution environment when you publish a function version.
--
-- 'optimizationStatus', 'snapStartResponse_optimizationStatus' - When you provide a
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html#versioning-versions-using qualified Amazon Resource Name (ARN)>,
-- this response element indicates whether SnapStart is activated for the
-- specified function version.
newSnapStartResponse ::
  SnapStartResponse
newSnapStartResponse =
  SnapStartResponse'
    { applyOn = Prelude.Nothing,
      optimizationStatus = Prelude.Nothing
    }

-- | When set to @PublishedVersions@, Lambda creates a snapshot of the
-- execution environment when you publish a function version.
snapStartResponse_applyOn :: Lens.Lens' SnapStartResponse (Prelude.Maybe SnapStartApplyOn)
snapStartResponse_applyOn = Lens.lens (\SnapStartResponse' {applyOn} -> applyOn) (\s@SnapStartResponse' {} a -> s {applyOn = a} :: SnapStartResponse)

-- | When you provide a
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-versions.html#versioning-versions-using qualified Amazon Resource Name (ARN)>,
-- this response element indicates whether SnapStart is activated for the
-- specified function version.
snapStartResponse_optimizationStatus :: Lens.Lens' SnapStartResponse (Prelude.Maybe SnapStartOptimizationStatus)
snapStartResponse_optimizationStatus = Lens.lens (\SnapStartResponse' {optimizationStatus} -> optimizationStatus) (\s@SnapStartResponse' {} a -> s {optimizationStatus = a} :: SnapStartResponse)

instance Data.FromJSON SnapStartResponse where
  parseJSON =
    Data.withObject
      "SnapStartResponse"
      ( \x ->
          SnapStartResponse'
            Prelude.<$> (x Data..:? "ApplyOn")
            Prelude.<*> (x Data..:? "OptimizationStatus")
      )

instance Prelude.Hashable SnapStartResponse where
  hashWithSalt _salt SnapStartResponse' {..} =
    _salt
      `Prelude.hashWithSalt` applyOn
      `Prelude.hashWithSalt` optimizationStatus

instance Prelude.NFData SnapStartResponse where
  rnf SnapStartResponse' {..} =
    Prelude.rnf applyOn `Prelude.seq`
      Prelude.rnf optimizationStatus
