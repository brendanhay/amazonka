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
-- Module      : Network.AWS.ECS.Types.Failure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Failure where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A failed resource. For a list of common causes, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/api_failures_messages.html API failure reasons>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newFailure' smart constructor.
data Failure = Failure'
  { -- | The Amazon Resource Name (ARN) of the failed resource.
    arn :: Core.Maybe Core.Text,
    -- | The reason for the failure.
    reason :: Core.Maybe Core.Text,
    -- | The details of the failure.
    detail :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Failure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'failure_arn' - The Amazon Resource Name (ARN) of the failed resource.
--
-- 'reason', 'failure_reason' - The reason for the failure.
--
-- 'detail', 'failure_detail' - The details of the failure.
newFailure ::
  Failure
newFailure =
  Failure'
    { arn = Core.Nothing,
      reason = Core.Nothing,
      detail = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the failed resource.
failure_arn :: Lens.Lens' Failure (Core.Maybe Core.Text)
failure_arn = Lens.lens (\Failure' {arn} -> arn) (\s@Failure' {} a -> s {arn = a} :: Failure)

-- | The reason for the failure.
failure_reason :: Lens.Lens' Failure (Core.Maybe Core.Text)
failure_reason = Lens.lens (\Failure' {reason} -> reason) (\s@Failure' {} a -> s {reason = a} :: Failure)

-- | The details of the failure.
failure_detail :: Lens.Lens' Failure (Core.Maybe Core.Text)
failure_detail = Lens.lens (\Failure' {detail} -> detail) (\s@Failure' {} a -> s {detail = a} :: Failure)

instance Core.FromJSON Failure where
  parseJSON =
    Core.withObject
      "Failure"
      ( \x ->
          Failure'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "reason")
            Core.<*> (x Core..:? "detail")
      )

instance Core.Hashable Failure

instance Core.NFData Failure
