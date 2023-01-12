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
-- Module      : Amazonka.DevOpsGuru.Types.LogAnomalyShowcase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.LogAnomalyShowcase where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.LogAnomalyClass
import qualified Amazonka.Prelude as Prelude

-- | A cluster of similar anomalous log events found within a log group.
--
-- /See:/ 'newLogAnomalyShowcase' smart constructor.
data LogAnomalyShowcase = LogAnomalyShowcase'
  { -- | A list of anomalous log events that may be related.
    logAnomalyClasses :: Prelude.Maybe [LogAnomalyClass]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogAnomalyShowcase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logAnomalyClasses', 'logAnomalyShowcase_logAnomalyClasses' - A list of anomalous log events that may be related.
newLogAnomalyShowcase ::
  LogAnomalyShowcase
newLogAnomalyShowcase =
  LogAnomalyShowcase'
    { logAnomalyClasses =
        Prelude.Nothing
    }

-- | A list of anomalous log events that may be related.
logAnomalyShowcase_logAnomalyClasses :: Lens.Lens' LogAnomalyShowcase (Prelude.Maybe [LogAnomalyClass])
logAnomalyShowcase_logAnomalyClasses = Lens.lens (\LogAnomalyShowcase' {logAnomalyClasses} -> logAnomalyClasses) (\s@LogAnomalyShowcase' {} a -> s {logAnomalyClasses = a} :: LogAnomalyShowcase) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LogAnomalyShowcase where
  parseJSON =
    Data.withObject
      "LogAnomalyShowcase"
      ( \x ->
          LogAnomalyShowcase'
            Prelude.<$> ( x Data..:? "LogAnomalyClasses"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LogAnomalyShowcase where
  hashWithSalt _salt LogAnomalyShowcase' {..} =
    _salt `Prelude.hashWithSalt` logAnomalyClasses

instance Prelude.NFData LogAnomalyShowcase where
  rnf LogAnomalyShowcase' {..} =
    Prelude.rnf logAnomalyClasses
