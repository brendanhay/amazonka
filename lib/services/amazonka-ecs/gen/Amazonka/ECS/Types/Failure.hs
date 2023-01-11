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
-- Module      : Amazonka.ECS.Types.Failure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Failure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A failed resource. For a list of common causes, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/api_failures_messages.html API failure reasons>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newFailure' smart constructor.
data Failure = Failure'
  { -- | The Amazon Resource Name (ARN) of the failed resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The details of the failure.
    detail :: Prelude.Maybe Prelude.Text,
    -- | The reason for the failure.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'detail', 'failure_detail' - The details of the failure.
--
-- 'reason', 'failure_reason' - The reason for the failure.
newFailure ::
  Failure
newFailure =
  Failure'
    { arn = Prelude.Nothing,
      detail = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the failed resource.
failure_arn :: Lens.Lens' Failure (Prelude.Maybe Prelude.Text)
failure_arn = Lens.lens (\Failure' {arn} -> arn) (\s@Failure' {} a -> s {arn = a} :: Failure)

-- | The details of the failure.
failure_detail :: Lens.Lens' Failure (Prelude.Maybe Prelude.Text)
failure_detail = Lens.lens (\Failure' {detail} -> detail) (\s@Failure' {} a -> s {detail = a} :: Failure)

-- | The reason for the failure.
failure_reason :: Lens.Lens' Failure (Prelude.Maybe Prelude.Text)
failure_reason = Lens.lens (\Failure' {reason} -> reason) (\s@Failure' {} a -> s {reason = a} :: Failure)

instance Data.FromJSON Failure where
  parseJSON =
    Data.withObject
      "Failure"
      ( \x ->
          Failure'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "detail")
            Prelude.<*> (x Data..:? "reason")
      )

instance Prelude.Hashable Failure where
  hashWithSalt _salt Failure' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` detail
      `Prelude.hashWithSalt` reason

instance Prelude.NFData Failure where
  rnf Failure' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf detail
      `Prelude.seq` Prelude.rnf reason
