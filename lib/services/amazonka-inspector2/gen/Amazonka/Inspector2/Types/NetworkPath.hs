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
-- Module      : Amazonka.Inspector2.Types.NetworkPath
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.NetworkPath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Step
import qualified Amazonka.Prelude as Prelude

-- | Information on the network path associated with a finding.
--
-- /See:/ 'newNetworkPath' smart constructor.
data NetworkPath = NetworkPath'
  { -- | The details on the steps in the network path.
    steps :: Prelude.Maybe (Prelude.NonEmpty Step)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'steps', 'networkPath_steps' - The details on the steps in the network path.
newNetworkPath ::
  NetworkPath
newNetworkPath =
  NetworkPath' {steps = Prelude.Nothing}

-- | The details on the steps in the network path.
networkPath_steps :: Lens.Lens' NetworkPath (Prelude.Maybe (Prelude.NonEmpty Step))
networkPath_steps = Lens.lens (\NetworkPath' {steps} -> steps) (\s@NetworkPath' {} a -> s {steps = a} :: NetworkPath) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON NetworkPath where
  parseJSON =
    Data.withObject
      "NetworkPath"
      ( \x ->
          NetworkPath' Prelude.<$> (x Data..:? "steps")
      )

instance Prelude.Hashable NetworkPath where
  hashWithSalt _salt NetworkPath' {..} =
    _salt `Prelude.hashWithSalt` steps

instance Prelude.NFData NetworkPath where
  rnf NetworkPath' {..} = Prelude.rnf steps
