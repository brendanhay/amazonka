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
-- Module      : Amazonka.SageMaker.Types.AutoMLPartialFailureReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLPartialFailureReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The reason for a partial failure of an AutoML job.
--
-- /See:/ 'newAutoMLPartialFailureReason' smart constructor.
data AutoMLPartialFailureReason = AutoMLPartialFailureReason'
  { -- | The message containing the reason for a partial failure of an AutoML
    -- job.
    partialFailureMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLPartialFailureReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partialFailureMessage', 'autoMLPartialFailureReason_partialFailureMessage' - The message containing the reason for a partial failure of an AutoML
-- job.
newAutoMLPartialFailureReason ::
  AutoMLPartialFailureReason
newAutoMLPartialFailureReason =
  AutoMLPartialFailureReason'
    { partialFailureMessage =
        Prelude.Nothing
    }

-- | The message containing the reason for a partial failure of an AutoML
-- job.
autoMLPartialFailureReason_partialFailureMessage :: Lens.Lens' AutoMLPartialFailureReason (Prelude.Maybe Prelude.Text)
autoMLPartialFailureReason_partialFailureMessage = Lens.lens (\AutoMLPartialFailureReason' {partialFailureMessage} -> partialFailureMessage) (\s@AutoMLPartialFailureReason' {} a -> s {partialFailureMessage = a} :: AutoMLPartialFailureReason)

instance Data.FromJSON AutoMLPartialFailureReason where
  parseJSON =
    Data.withObject
      "AutoMLPartialFailureReason"
      ( \x ->
          AutoMLPartialFailureReason'
            Prelude.<$> (x Data..:? "PartialFailureMessage")
      )

instance Prelude.Hashable AutoMLPartialFailureReason where
  hashWithSalt _salt AutoMLPartialFailureReason' {..} =
    _salt `Prelude.hashWithSalt` partialFailureMessage

instance Prelude.NFData AutoMLPartialFailureReason where
  rnf AutoMLPartialFailureReason' {..} =
    Prelude.rnf partialFailureMessage
