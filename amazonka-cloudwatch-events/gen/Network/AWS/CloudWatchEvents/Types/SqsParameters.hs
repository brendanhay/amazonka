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
-- Module      : Network.AWS.CloudWatchEvents.Types.SqsParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.SqsParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This structure includes the custom parameter to be used when the target
-- is an SQS FIFO queue.
--
-- /See:/ 'newSqsParameters' smart constructor.
data SqsParameters = SqsParameters'
  { -- | The FIFO message group ID to use as the target.
    messageGroupId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SqsParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageGroupId', 'sqsParameters_messageGroupId' - The FIFO message group ID to use as the target.
newSqsParameters ::
  SqsParameters
newSqsParameters =
  SqsParameters' {messageGroupId = Core.Nothing}

-- | The FIFO message group ID to use as the target.
sqsParameters_messageGroupId :: Lens.Lens' SqsParameters (Core.Maybe Core.Text)
sqsParameters_messageGroupId = Lens.lens (\SqsParameters' {messageGroupId} -> messageGroupId) (\s@SqsParameters' {} a -> s {messageGroupId = a} :: SqsParameters)

instance Core.FromJSON SqsParameters where
  parseJSON =
    Core.withObject
      "SqsParameters"
      ( \x ->
          SqsParameters'
            Core.<$> (x Core..:? "MessageGroupId")
      )

instance Core.Hashable SqsParameters

instance Core.NFData SqsParameters

instance Core.ToJSON SqsParameters where
  toJSON SqsParameters' {..} =
    Core.object
      ( Core.catMaybes
          [("MessageGroupId" Core..=) Core.<$> messageGroupId]
      )
