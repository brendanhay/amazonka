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
-- Module      : Amazonka.Athena.Types.ResultReuseByAgeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ResultReuseByAgeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether previous query results are reused, and if so, their
-- maximum age.
--
-- /See:/ 'newResultReuseByAgeConfiguration' smart constructor.
data ResultReuseByAgeConfiguration = ResultReuseByAgeConfiguration'
  { -- | Specifies, in minutes, the maximum age of a previous query result that
    -- Athena should consider for reuse. The default is 60.
    maxAgeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | True if previous query results can be reused when the query is run;
    -- otherwise, false. The default is false.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultReuseByAgeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxAgeInMinutes', 'resultReuseByAgeConfiguration_maxAgeInMinutes' - Specifies, in minutes, the maximum age of a previous query result that
-- Athena should consider for reuse. The default is 60.
--
-- 'enabled', 'resultReuseByAgeConfiguration_enabled' - True if previous query results can be reused when the query is run;
-- otherwise, false. The default is false.
newResultReuseByAgeConfiguration ::
  -- | 'enabled'
  Prelude.Bool ->
  ResultReuseByAgeConfiguration
newResultReuseByAgeConfiguration pEnabled_ =
  ResultReuseByAgeConfiguration'
    { maxAgeInMinutes =
        Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Specifies, in minutes, the maximum age of a previous query result that
-- Athena should consider for reuse. The default is 60.
resultReuseByAgeConfiguration_maxAgeInMinutes :: Lens.Lens' ResultReuseByAgeConfiguration (Prelude.Maybe Prelude.Natural)
resultReuseByAgeConfiguration_maxAgeInMinutes = Lens.lens (\ResultReuseByAgeConfiguration' {maxAgeInMinutes} -> maxAgeInMinutes) (\s@ResultReuseByAgeConfiguration' {} a -> s {maxAgeInMinutes = a} :: ResultReuseByAgeConfiguration)

-- | True if previous query results can be reused when the query is run;
-- otherwise, false. The default is false.
resultReuseByAgeConfiguration_enabled :: Lens.Lens' ResultReuseByAgeConfiguration Prelude.Bool
resultReuseByAgeConfiguration_enabled = Lens.lens (\ResultReuseByAgeConfiguration' {enabled} -> enabled) (\s@ResultReuseByAgeConfiguration' {} a -> s {enabled = a} :: ResultReuseByAgeConfiguration)

instance Core.FromJSON ResultReuseByAgeConfiguration where
  parseJSON =
    Core.withObject
      "ResultReuseByAgeConfiguration"
      ( \x ->
          ResultReuseByAgeConfiguration'
            Prelude.<$> (x Core..:? "MaxAgeInMinutes")
            Prelude.<*> (x Core..: "Enabled")
      )

instance
  Prelude.Hashable
    ResultReuseByAgeConfiguration
  where
  hashWithSalt _salt ResultReuseByAgeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` maxAgeInMinutes
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData ResultReuseByAgeConfiguration where
  rnf ResultReuseByAgeConfiguration' {..} =
    Prelude.rnf maxAgeInMinutes
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToJSON ResultReuseByAgeConfiguration where
  toJSON ResultReuseByAgeConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxAgeInMinutes" Core..=)
              Prelude.<$> maxAgeInMinutes,
            Prelude.Just ("Enabled" Core..= enabled)
          ]
      )
