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
-- Module      : Amazonka.SQS.Types.DeleteMessageBatchResultEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Types.DeleteMessageBatchResultEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Encloses the @Id@ of an entry in @ DeleteMessageBatch.@
--
-- /See:/ 'newDeleteMessageBatchResultEntry' smart constructor.
data DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry'
  { -- | Represents a successfully deleted message.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessageBatchResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteMessageBatchResultEntry_id' - Represents a successfully deleted message.
newDeleteMessageBatchResultEntry ::
  -- | 'id'
  Prelude.Text ->
  DeleteMessageBatchResultEntry
newDeleteMessageBatchResultEntry pId_ =
  DeleteMessageBatchResultEntry' {id = pId_}

-- | Represents a successfully deleted message.
deleteMessageBatchResultEntry_id :: Lens.Lens' DeleteMessageBatchResultEntry Prelude.Text
deleteMessageBatchResultEntry_id = Lens.lens (\DeleteMessageBatchResultEntry' {id} -> id) (\s@DeleteMessageBatchResultEntry' {} a -> s {id = a} :: DeleteMessageBatchResultEntry)

instance Core.FromXML DeleteMessageBatchResultEntry where
  parseXML x =
    DeleteMessageBatchResultEntry'
      Prelude.<$> (x Core..@ "Id")

instance
  Prelude.Hashable
    DeleteMessageBatchResultEntry
  where
  hashWithSalt _salt DeleteMessageBatchResultEntry' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteMessageBatchResultEntry where
  rnf DeleteMessageBatchResultEntry' {..} =
    Prelude.rnf id
