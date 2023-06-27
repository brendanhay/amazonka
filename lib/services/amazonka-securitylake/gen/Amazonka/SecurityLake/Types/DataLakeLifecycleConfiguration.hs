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
-- Module      : Amazonka.SecurityLake.Types.DataLakeLifecycleConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeLifecycleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.DataLakeLifecycleExpiration
import Amazonka.SecurityLake.Types.DataLakeLifecycleTransition

-- | Provides lifecycle details of Amazon Security Lake object.
--
-- /See:/ 'newDataLakeLifecycleConfiguration' smart constructor.
data DataLakeLifecycleConfiguration = DataLakeLifecycleConfiguration'
  { -- | Provides data expiration details of Amazon Security Lake object.
    expiration :: Prelude.Maybe DataLakeLifecycleExpiration,
    -- | Provides data storage transition details of Amazon Security Lake object.
    transitions :: Prelude.Maybe [DataLakeLifecycleTransition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeLifecycleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiration', 'dataLakeLifecycleConfiguration_expiration' - Provides data expiration details of Amazon Security Lake object.
--
-- 'transitions', 'dataLakeLifecycleConfiguration_transitions' - Provides data storage transition details of Amazon Security Lake object.
newDataLakeLifecycleConfiguration ::
  DataLakeLifecycleConfiguration
newDataLakeLifecycleConfiguration =
  DataLakeLifecycleConfiguration'
    { expiration =
        Prelude.Nothing,
      transitions = Prelude.Nothing
    }

-- | Provides data expiration details of Amazon Security Lake object.
dataLakeLifecycleConfiguration_expiration :: Lens.Lens' DataLakeLifecycleConfiguration (Prelude.Maybe DataLakeLifecycleExpiration)
dataLakeLifecycleConfiguration_expiration = Lens.lens (\DataLakeLifecycleConfiguration' {expiration} -> expiration) (\s@DataLakeLifecycleConfiguration' {} a -> s {expiration = a} :: DataLakeLifecycleConfiguration)

-- | Provides data storage transition details of Amazon Security Lake object.
dataLakeLifecycleConfiguration_transitions :: Lens.Lens' DataLakeLifecycleConfiguration (Prelude.Maybe [DataLakeLifecycleTransition])
dataLakeLifecycleConfiguration_transitions = Lens.lens (\DataLakeLifecycleConfiguration' {transitions} -> transitions) (\s@DataLakeLifecycleConfiguration' {} a -> s {transitions = a} :: DataLakeLifecycleConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DataLakeLifecycleConfiguration where
  parseJSON =
    Data.withObject
      "DataLakeLifecycleConfiguration"
      ( \x ->
          DataLakeLifecycleConfiguration'
            Prelude.<$> (x Data..:? "expiration")
            Prelude.<*> (x Data..:? "transitions" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    DataLakeLifecycleConfiguration
  where
  hashWithSalt
    _salt
    DataLakeLifecycleConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` expiration
        `Prelude.hashWithSalt` transitions

instance
  Prelude.NFData
    DataLakeLifecycleConfiguration
  where
  rnf DataLakeLifecycleConfiguration' {..} =
    Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf transitions

instance Data.ToJSON DataLakeLifecycleConfiguration where
  toJSON DataLakeLifecycleConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expiration" Data..=) Prelude.<$> expiration,
            ("transitions" Data..=) Prelude.<$> transitions
          ]
      )
