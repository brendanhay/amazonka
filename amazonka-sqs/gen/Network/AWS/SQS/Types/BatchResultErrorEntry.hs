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
-- Module      : Network.AWS.SQS.Types.BatchResultErrorEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.BatchResultErrorEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Gives a detailed description of the result of an action on each entry in
-- the request.
--
-- /See:/ 'newBatchResultErrorEntry' smart constructor.
data BatchResultErrorEntry = BatchResultErrorEntry'
  { -- | A message explaining why the action failed on this entry.
    message :: Core.Maybe Core.Text,
    -- | The @Id@ of an entry in a batch request.
    id :: Core.Text,
    -- | Specifies whether the error happened due to the caller of the batch API
    -- action.
    senderFault :: Core.Bool,
    -- | An error code representing why the action failed on this entry.
    code :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchResultErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'batchResultErrorEntry_message' - A message explaining why the action failed on this entry.
--
-- 'id', 'batchResultErrorEntry_id' - The @Id@ of an entry in a batch request.
--
-- 'senderFault', 'batchResultErrorEntry_senderFault' - Specifies whether the error happened due to the caller of the batch API
-- action.
--
-- 'code', 'batchResultErrorEntry_code' - An error code representing why the action failed on this entry.
newBatchResultErrorEntry ::
  -- | 'id'
  Core.Text ->
  -- | 'senderFault'
  Core.Bool ->
  -- | 'code'
  Core.Text ->
  BatchResultErrorEntry
newBatchResultErrorEntry pId_ pSenderFault_ pCode_ =
  BatchResultErrorEntry'
    { message = Core.Nothing,
      id = pId_,
      senderFault = pSenderFault_,
      code = pCode_
    }

-- | A message explaining why the action failed on this entry.
batchResultErrorEntry_message :: Lens.Lens' BatchResultErrorEntry (Core.Maybe Core.Text)
batchResultErrorEntry_message = Lens.lens (\BatchResultErrorEntry' {message} -> message) (\s@BatchResultErrorEntry' {} a -> s {message = a} :: BatchResultErrorEntry)

-- | The @Id@ of an entry in a batch request.
batchResultErrorEntry_id :: Lens.Lens' BatchResultErrorEntry Core.Text
batchResultErrorEntry_id = Lens.lens (\BatchResultErrorEntry' {id} -> id) (\s@BatchResultErrorEntry' {} a -> s {id = a} :: BatchResultErrorEntry)

-- | Specifies whether the error happened due to the caller of the batch API
-- action.
batchResultErrorEntry_senderFault :: Lens.Lens' BatchResultErrorEntry Core.Bool
batchResultErrorEntry_senderFault = Lens.lens (\BatchResultErrorEntry' {senderFault} -> senderFault) (\s@BatchResultErrorEntry' {} a -> s {senderFault = a} :: BatchResultErrorEntry)

-- | An error code representing why the action failed on this entry.
batchResultErrorEntry_code :: Lens.Lens' BatchResultErrorEntry Core.Text
batchResultErrorEntry_code = Lens.lens (\BatchResultErrorEntry' {code} -> code) (\s@BatchResultErrorEntry' {} a -> s {code = a} :: BatchResultErrorEntry)

instance Core.FromXML BatchResultErrorEntry where
  parseXML x =
    BatchResultErrorEntry'
      Core.<$> (x Core..@? "Message")
      Core.<*> (x Core..@ "Id")
      Core.<*> (x Core..@ "SenderFault")
      Core.<*> (x Core..@ "Code")

instance Core.Hashable BatchResultErrorEntry

instance Core.NFData BatchResultErrorEntry
