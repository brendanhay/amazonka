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
-- Module      : Amazonka.Athena.Types.ResultReuseConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ResultReuseConfiguration where

import Amazonka.Athena.Types.ResultReuseByAgeConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the query result reuse behavior for the query.
--
-- /See:/ 'newResultReuseConfiguration' smart constructor.
data ResultReuseConfiguration = ResultReuseConfiguration'
  { -- | Specifies whether previous query results are reused, and if so, their
    -- maximum age.
    resultReuseByAgeConfiguration :: Prelude.Maybe ResultReuseByAgeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultReuseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resultReuseByAgeConfiguration', 'resultReuseConfiguration_resultReuseByAgeConfiguration' - Specifies whether previous query results are reused, and if so, their
-- maximum age.
newResultReuseConfiguration ::
  ResultReuseConfiguration
newResultReuseConfiguration =
  ResultReuseConfiguration'
    { resultReuseByAgeConfiguration =
        Prelude.Nothing
    }

-- | Specifies whether previous query results are reused, and if so, their
-- maximum age.
resultReuseConfiguration_resultReuseByAgeConfiguration :: Lens.Lens' ResultReuseConfiguration (Prelude.Maybe ResultReuseByAgeConfiguration)
resultReuseConfiguration_resultReuseByAgeConfiguration = Lens.lens (\ResultReuseConfiguration' {resultReuseByAgeConfiguration} -> resultReuseByAgeConfiguration) (\s@ResultReuseConfiguration' {} a -> s {resultReuseByAgeConfiguration = a} :: ResultReuseConfiguration)

instance Data.FromJSON ResultReuseConfiguration where
  parseJSON =
    Data.withObject
      "ResultReuseConfiguration"
      ( \x ->
          ResultReuseConfiguration'
            Prelude.<$> (x Data..:? "ResultReuseByAgeConfiguration")
      )

instance Prelude.Hashable ResultReuseConfiguration where
  hashWithSalt _salt ResultReuseConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` resultReuseByAgeConfiguration

instance Prelude.NFData ResultReuseConfiguration where
  rnf ResultReuseConfiguration' {..} =
    Prelude.rnf resultReuseByAgeConfiguration

instance Data.ToJSON ResultReuseConfiguration where
  toJSON ResultReuseConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResultReuseByAgeConfiguration" Data..=)
              Prelude.<$> resultReuseByAgeConfiguration
          ]
      )
