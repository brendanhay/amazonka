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
-- Module      : Amazonka.Batch.Types.EksAttemptContainerDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksAttemptContainerDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the details for an attempt for a job attempt
-- that an Amazon EKS container runs.
--
-- /See:/ 'newEksAttemptContainerDetail' smart constructor.
data EksAttemptContainerDetail = EksAttemptContainerDetail'
  { -- | The exit code for the job attempt. A non-zero exit code is considered
    -- failed.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | A short (255 max characters) human-readable string to provide additional
    -- details for a running or stopped container.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksAttemptContainerDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exitCode', 'eksAttemptContainerDetail_exitCode' - The exit code for the job attempt. A non-zero exit code is considered
-- failed.
--
-- 'reason', 'eksAttemptContainerDetail_reason' - A short (255 max characters) human-readable string to provide additional
-- details for a running or stopped container.
newEksAttemptContainerDetail ::
  EksAttemptContainerDetail
newEksAttemptContainerDetail =
  EksAttemptContainerDetail'
    { exitCode =
        Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The exit code for the job attempt. A non-zero exit code is considered
-- failed.
eksAttemptContainerDetail_exitCode :: Lens.Lens' EksAttemptContainerDetail (Prelude.Maybe Prelude.Int)
eksAttemptContainerDetail_exitCode = Lens.lens (\EksAttemptContainerDetail' {exitCode} -> exitCode) (\s@EksAttemptContainerDetail' {} a -> s {exitCode = a} :: EksAttemptContainerDetail)

-- | A short (255 max characters) human-readable string to provide additional
-- details for a running or stopped container.
eksAttemptContainerDetail_reason :: Lens.Lens' EksAttemptContainerDetail (Prelude.Maybe Prelude.Text)
eksAttemptContainerDetail_reason = Lens.lens (\EksAttemptContainerDetail' {reason} -> reason) (\s@EksAttemptContainerDetail' {} a -> s {reason = a} :: EksAttemptContainerDetail)

instance Data.FromJSON EksAttemptContainerDetail where
  parseJSON =
    Data.withObject
      "EksAttemptContainerDetail"
      ( \x ->
          EksAttemptContainerDetail'
            Prelude.<$> (x Data..:? "exitCode")
            Prelude.<*> (x Data..:? "reason")
      )

instance Prelude.Hashable EksAttemptContainerDetail where
  hashWithSalt _salt EksAttemptContainerDetail' {..} =
    _salt
      `Prelude.hashWithSalt` exitCode
      `Prelude.hashWithSalt` reason

instance Prelude.NFData EksAttemptContainerDetail where
  rnf EksAttemptContainerDetail' {..} =
    Prelude.rnf exitCode
      `Prelude.seq` Prelude.rnf reason
