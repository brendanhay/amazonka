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
-- Module      : Amazonka.MGN.Types.LifeCycleLastCutoverReverted
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycleLastCutoverReverted where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle last Cutover reverted.
--
-- /See:/ 'newLifeCycleLastCutoverReverted' smart constructor.
data LifeCycleLastCutoverReverted = LifeCycleLastCutoverReverted'
  { -- | Lifecycle last Cutover reverted API call date time.
    apiCallDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycleLastCutoverReverted' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCallDateTime', 'lifeCycleLastCutoverReverted_apiCallDateTime' - Lifecycle last Cutover reverted API call date time.
newLifeCycleLastCutoverReverted ::
  LifeCycleLastCutoverReverted
newLifeCycleLastCutoverReverted =
  LifeCycleLastCutoverReverted'
    { apiCallDateTime =
        Prelude.Nothing
    }

-- | Lifecycle last Cutover reverted API call date time.
lifeCycleLastCutoverReverted_apiCallDateTime :: Lens.Lens' LifeCycleLastCutoverReverted (Prelude.Maybe Prelude.Text)
lifeCycleLastCutoverReverted_apiCallDateTime = Lens.lens (\LifeCycleLastCutoverReverted' {apiCallDateTime} -> apiCallDateTime) (\s@LifeCycleLastCutoverReverted' {} a -> s {apiCallDateTime = a} :: LifeCycleLastCutoverReverted)

instance Core.FromJSON LifeCycleLastCutoverReverted where
  parseJSON =
    Core.withObject
      "LifeCycleLastCutoverReverted"
      ( \x ->
          LifeCycleLastCutoverReverted'
            Prelude.<$> (x Core..:? "apiCallDateTime")
      )

instance
  Prelude.Hashable
    LifeCycleLastCutoverReverted
  where
  hashWithSalt _salt LifeCycleLastCutoverReverted' {..} =
    _salt `Prelude.hashWithSalt` apiCallDateTime

instance Prelude.NFData LifeCycleLastCutoverReverted where
  rnf LifeCycleLastCutoverReverted' {..} =
    Prelude.rnf apiCallDateTime
