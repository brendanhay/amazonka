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
-- Module      : Amazonka.Config.Types.ExecutionControls
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ExecutionControls where

import Amazonka.Config.Types.SsmControls
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The controls that Config uses for executing remediations.
--
-- /See:/ 'newExecutionControls' smart constructor.
data ExecutionControls = ExecutionControls'
  { -- | A SsmControls object.
    ssmControls :: Prelude.Maybe SsmControls
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ssmControls', 'executionControls_ssmControls' - A SsmControls object.
newExecutionControls ::
  ExecutionControls
newExecutionControls =
  ExecutionControls' {ssmControls = Prelude.Nothing}

-- | A SsmControls object.
executionControls_ssmControls :: Lens.Lens' ExecutionControls (Prelude.Maybe SsmControls)
executionControls_ssmControls = Lens.lens (\ExecutionControls' {ssmControls} -> ssmControls) (\s@ExecutionControls' {} a -> s {ssmControls = a} :: ExecutionControls)

instance Data.FromJSON ExecutionControls where
  parseJSON =
    Data.withObject
      "ExecutionControls"
      ( \x ->
          ExecutionControls'
            Prelude.<$> (x Data..:? "SsmControls")
      )

instance Prelude.Hashable ExecutionControls where
  hashWithSalt _salt ExecutionControls' {..} =
    _salt `Prelude.hashWithSalt` ssmControls

instance Prelude.NFData ExecutionControls where
  rnf ExecutionControls' {..} = Prelude.rnf ssmControls

instance Data.ToJSON ExecutionControls where
  toJSON ExecutionControls' {..} =
    Data.object
      ( Prelude.catMaybes
          [("SsmControls" Data..=) Prelude.<$> ssmControls]
      )
