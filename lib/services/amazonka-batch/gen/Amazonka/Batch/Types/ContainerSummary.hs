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
-- Module      : Amazonka.Batch.Types.ContainerSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ContainerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents summary details of a container within a job.
--
-- /See:/ 'newContainerSummary' smart constructor.
data ContainerSummary = ContainerSummary'
  { -- | The exit code to return upon completion.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | A short (255 max characters) human-readable string to provide additional
    -- details for a running or stopped container.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exitCode', 'containerSummary_exitCode' - The exit code to return upon completion.
--
-- 'reason', 'containerSummary_reason' - A short (255 max characters) human-readable string to provide additional
-- details for a running or stopped container.
newContainerSummary ::
  ContainerSummary
newContainerSummary =
  ContainerSummary'
    { exitCode = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The exit code to return upon completion.
containerSummary_exitCode :: Lens.Lens' ContainerSummary (Prelude.Maybe Prelude.Int)
containerSummary_exitCode = Lens.lens (\ContainerSummary' {exitCode} -> exitCode) (\s@ContainerSummary' {} a -> s {exitCode = a} :: ContainerSummary)

-- | A short (255 max characters) human-readable string to provide additional
-- details for a running or stopped container.
containerSummary_reason :: Lens.Lens' ContainerSummary (Prelude.Maybe Prelude.Text)
containerSummary_reason = Lens.lens (\ContainerSummary' {reason} -> reason) (\s@ContainerSummary' {} a -> s {reason = a} :: ContainerSummary)

instance Data.FromJSON ContainerSummary where
  parseJSON =
    Data.withObject
      "ContainerSummary"
      ( \x ->
          ContainerSummary'
            Prelude.<$> (x Data..:? "exitCode")
            Prelude.<*> (x Data..:? "reason")
      )

instance Prelude.Hashable ContainerSummary where
  hashWithSalt _salt ContainerSummary' {..} =
    _salt `Prelude.hashWithSalt` exitCode
      `Prelude.hashWithSalt` reason

instance Prelude.NFData ContainerSummary where
  rnf ContainerSummary' {..} =
    Prelude.rnf exitCode
      `Prelude.seq` Prelude.rnf reason
