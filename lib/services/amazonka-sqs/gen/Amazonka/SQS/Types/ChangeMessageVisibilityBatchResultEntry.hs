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
-- Module      : Amazonka.SQS.Types.ChangeMessageVisibilityBatchResultEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Types.ChangeMessageVisibilityBatchResultEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Encloses the @Id@ of an entry in @ @@ChangeMessageVisibilityBatch@@.@
--
-- /See:/ 'newChangeMessageVisibilityBatchResultEntry' smart constructor.
data ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry'
  { -- | Represents a message whose visibility timeout has been changed
    -- successfully.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeMessageVisibilityBatchResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'changeMessageVisibilityBatchResultEntry_id' - Represents a message whose visibility timeout has been changed
-- successfully.
newChangeMessageVisibilityBatchResultEntry ::
  -- | 'id'
  Prelude.Text ->
  ChangeMessageVisibilityBatchResultEntry
newChangeMessageVisibilityBatchResultEntry pId_ =
  ChangeMessageVisibilityBatchResultEntry' {id = pId_}

-- | Represents a message whose visibility timeout has been changed
-- successfully.
changeMessageVisibilityBatchResultEntry_id :: Lens.Lens' ChangeMessageVisibilityBatchResultEntry Prelude.Text
changeMessageVisibilityBatchResultEntry_id = Lens.lens (\ChangeMessageVisibilityBatchResultEntry' {id} -> id) (\s@ChangeMessageVisibilityBatchResultEntry' {} a -> s {id = a} :: ChangeMessageVisibilityBatchResultEntry)

instance
  Data.FromXML
    ChangeMessageVisibilityBatchResultEntry
  where
  parseXML x =
    ChangeMessageVisibilityBatchResultEntry'
      Prelude.<$> (x Data..@ "Id")

instance
  Prelude.Hashable
    ChangeMessageVisibilityBatchResultEntry
  where
  hashWithSalt
    _salt
    ChangeMessageVisibilityBatchResultEntry' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    ChangeMessageVisibilityBatchResultEntry
  where
  rnf ChangeMessageVisibilityBatchResultEntry' {..} =
    Prelude.rnf id
