{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Batch.Types.ContainerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing summary details of a container within a job.
--
-- /See:/ 'newContainerSummary' smart constructor.
data ContainerSummary = ContainerSummary'
  { -- | The exit code to return upon completion.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | A short (255 max characters) human-readable string to provide additional
    -- details about a running or stopped container.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- details about a running or stopped container.
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
-- details about a running or stopped container.
containerSummary_reason :: Lens.Lens' ContainerSummary (Prelude.Maybe Prelude.Text)
containerSummary_reason = Lens.lens (\ContainerSummary' {reason} -> reason) (\s@ContainerSummary' {} a -> s {reason = a} :: ContainerSummary)

instance Prelude.FromJSON ContainerSummary where
  parseJSON =
    Prelude.withObject
      "ContainerSummary"
      ( \x ->
          ContainerSummary'
            Prelude.<$> (x Prelude..:? "exitCode")
            Prelude.<*> (x Prelude..:? "reason")
      )

instance Prelude.Hashable ContainerSummary

instance Prelude.NFData ContainerSummary
