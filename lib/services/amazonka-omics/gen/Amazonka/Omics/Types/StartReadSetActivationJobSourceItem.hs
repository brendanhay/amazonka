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
-- Module      : Amazonka.Omics.Types.StartReadSetActivationJobSourceItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.StartReadSetActivationJobSourceItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A source for a read set activation job.
--
-- /See:/ 'newStartReadSetActivationJobSourceItem' smart constructor.
data StartReadSetActivationJobSourceItem = StartReadSetActivationJobSourceItem'
  { -- | The source\'s read set ID.
    readSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReadSetActivationJobSourceItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readSetId', 'startReadSetActivationJobSourceItem_readSetId' - The source\'s read set ID.
newStartReadSetActivationJobSourceItem ::
  -- | 'readSetId'
  Prelude.Text ->
  StartReadSetActivationJobSourceItem
newStartReadSetActivationJobSourceItem pReadSetId_ =
  StartReadSetActivationJobSourceItem'
    { readSetId =
        pReadSetId_
    }

-- | The source\'s read set ID.
startReadSetActivationJobSourceItem_readSetId :: Lens.Lens' StartReadSetActivationJobSourceItem Prelude.Text
startReadSetActivationJobSourceItem_readSetId = Lens.lens (\StartReadSetActivationJobSourceItem' {readSetId} -> readSetId) (\s@StartReadSetActivationJobSourceItem' {} a -> s {readSetId = a} :: StartReadSetActivationJobSourceItem)

instance
  Prelude.Hashable
    StartReadSetActivationJobSourceItem
  where
  hashWithSalt
    _salt
    StartReadSetActivationJobSourceItem' {..} =
      _salt `Prelude.hashWithSalt` readSetId

instance
  Prelude.NFData
    StartReadSetActivationJobSourceItem
  where
  rnf StartReadSetActivationJobSourceItem' {..} =
    Prelude.rnf readSetId

instance
  Data.ToJSON
    StartReadSetActivationJobSourceItem
  where
  toJSON StartReadSetActivationJobSourceItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("readSetId" Data..= readSetId)]
      )
