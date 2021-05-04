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
-- Module      : Network.AWS.CloudWatchEvents.Types.SqsParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.SqsParameters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This structure includes the custom parameter to be used when the target
-- is an SQS FIFO queue.
--
-- /See:/ 'newSqsParameters' smart constructor.
data SqsParameters = SqsParameters'
  { -- | The FIFO message group ID to use as the target.
    messageGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  SqsParameters' {messageGroupId = Prelude.Nothing}

-- | The FIFO message group ID to use as the target.
sqsParameters_messageGroupId :: Lens.Lens' SqsParameters (Prelude.Maybe Prelude.Text)
sqsParameters_messageGroupId = Lens.lens (\SqsParameters' {messageGroupId} -> messageGroupId) (\s@SqsParameters' {} a -> s {messageGroupId = a} :: SqsParameters)

instance Prelude.FromJSON SqsParameters where
  parseJSON =
    Prelude.withObject
      "SqsParameters"
      ( \x ->
          SqsParameters'
            Prelude.<$> (x Prelude..:? "MessageGroupId")
      )

instance Prelude.Hashable SqsParameters

instance Prelude.NFData SqsParameters

instance Prelude.ToJSON SqsParameters where
  toJSON SqsParameters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MessageGroupId" Prelude..=)
              Prelude.<$> messageGroupId
          ]
      )
