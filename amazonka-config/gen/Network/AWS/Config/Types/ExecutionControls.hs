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
-- Module      : Network.AWS.Config.Types.ExecutionControls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ExecutionControls where

import Network.AWS.Config.Types.SsmControls
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The controls that AWS Config uses for executing remediations.
--
-- /See:/ 'newExecutionControls' smart constructor.
data ExecutionControls = ExecutionControls'
  { -- | A SsmControls object.
    ssmControls :: Prelude.Maybe SsmControls
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ExecutionControls where
  parseJSON =
    Prelude.withObject
      "ExecutionControls"
      ( \x ->
          ExecutionControls'
            Prelude.<$> (x Prelude..:? "SsmControls")
      )

instance Prelude.Hashable ExecutionControls

instance Prelude.NFData ExecutionControls

instance Prelude.ToJSON ExecutionControls where
  toJSON ExecutionControls' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("SsmControls" Prelude..=) Prelude.<$> ssmControls]
      )
