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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQueryResultConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQueryResultConfiguration where

import Amazonka.CleanRooms.Types.ProtectedQueryOutputConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains configurations for protected query results.
--
-- /See:/ 'newProtectedQueryResultConfiguration' smart constructor.
data ProtectedQueryResultConfiguration = ProtectedQueryResultConfiguration'
  { -- | Configuration for protected query results.
    outputConfiguration :: ProtectedQueryOutputConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQueryResultConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputConfiguration', 'protectedQueryResultConfiguration_outputConfiguration' - Configuration for protected query results.
newProtectedQueryResultConfiguration ::
  -- | 'outputConfiguration'
  ProtectedQueryOutputConfiguration ->
  ProtectedQueryResultConfiguration
newProtectedQueryResultConfiguration
  pOutputConfiguration_ =
    ProtectedQueryResultConfiguration'
      { outputConfiguration =
          pOutputConfiguration_
      }

-- | Configuration for protected query results.
protectedQueryResultConfiguration_outputConfiguration :: Lens.Lens' ProtectedQueryResultConfiguration ProtectedQueryOutputConfiguration
protectedQueryResultConfiguration_outputConfiguration = Lens.lens (\ProtectedQueryResultConfiguration' {outputConfiguration} -> outputConfiguration) (\s@ProtectedQueryResultConfiguration' {} a -> s {outputConfiguration = a} :: ProtectedQueryResultConfiguration)

instance
  Data.FromJSON
    ProtectedQueryResultConfiguration
  where
  parseJSON =
    Data.withObject
      "ProtectedQueryResultConfiguration"
      ( \x ->
          ProtectedQueryResultConfiguration'
            Prelude.<$> (x Data..: "outputConfiguration")
      )

instance
  Prelude.Hashable
    ProtectedQueryResultConfiguration
  where
  hashWithSalt
    _salt
    ProtectedQueryResultConfiguration' {..} =
      _salt `Prelude.hashWithSalt` outputConfiguration

instance
  Prelude.NFData
    ProtectedQueryResultConfiguration
  where
  rnf ProtectedQueryResultConfiguration' {..} =
    Prelude.rnf outputConfiguration

instance
  Data.ToJSON
    ProtectedQueryResultConfiguration
  where
  toJSON ProtectedQueryResultConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("outputConfiguration" Data..= outputConfiguration)
          ]
      )
