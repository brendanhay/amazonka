{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SQS.Types.DeleteMessageBatchResultEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.DeleteMessageBatchResultEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Encloses the @Id@ of an entry in @ DeleteMessageBatch.@
--
-- /See:/ 'newDeleteMessageBatchResultEntry' smart constructor.
data DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry'
  { -- | Represents a successfully deleted message.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromXML
    DeleteMessageBatchResultEntry
  where
  parseXML x =
    DeleteMessageBatchResultEntry'
      Prelude.<$> (x Prelude..@ "Id")

instance
  Prelude.Hashable
    DeleteMessageBatchResultEntry

instance Prelude.NFData DeleteMessageBatchResultEntry
