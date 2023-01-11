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
-- Module      : Amazonka.MGN.Types.LifeCycleLastTestFinalized
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycleLastTestFinalized where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle last Test finalized.
--
-- /See:/ 'newLifeCycleLastTestFinalized' smart constructor.
data LifeCycleLastTestFinalized = LifeCycleLastTestFinalized'
  { -- | Lifecycle Test failed API call date and time.
    apiCallDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycleLastTestFinalized' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCallDateTime', 'lifeCycleLastTestFinalized_apiCallDateTime' - Lifecycle Test failed API call date and time.
newLifeCycleLastTestFinalized ::
  LifeCycleLastTestFinalized
newLifeCycleLastTestFinalized =
  LifeCycleLastTestFinalized'
    { apiCallDateTime =
        Prelude.Nothing
    }

-- | Lifecycle Test failed API call date and time.
lifeCycleLastTestFinalized_apiCallDateTime :: Lens.Lens' LifeCycleLastTestFinalized (Prelude.Maybe Prelude.Text)
lifeCycleLastTestFinalized_apiCallDateTime = Lens.lens (\LifeCycleLastTestFinalized' {apiCallDateTime} -> apiCallDateTime) (\s@LifeCycleLastTestFinalized' {} a -> s {apiCallDateTime = a} :: LifeCycleLastTestFinalized)

instance Data.FromJSON LifeCycleLastTestFinalized where
  parseJSON =
    Data.withObject
      "LifeCycleLastTestFinalized"
      ( \x ->
          LifeCycleLastTestFinalized'
            Prelude.<$> (x Data..:? "apiCallDateTime")
      )

instance Prelude.Hashable LifeCycleLastTestFinalized where
  hashWithSalt _salt LifeCycleLastTestFinalized' {..} =
    _salt `Prelude.hashWithSalt` apiCallDateTime

instance Prelude.NFData LifeCycleLastTestFinalized where
  rnf LifeCycleLastTestFinalized' {..} =
    Prelude.rnf apiCallDateTime
