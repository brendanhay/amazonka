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
-- Module      : Network.AWS.CloudWatch.Types.MessageData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MessageData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A message returned by the @GetMetricData@API, including a code and a
-- description.
--
-- If a cross-Region @GetMetricData@ operation fails with a code of
-- @Forbidden@ and a value of
-- @Authentication too complex to retrieve cross region data@, you can
-- correct the problem by running the @GetMetricData@ operation in the same
-- Region where the metric data is.
--
-- /See:/ 'newMessageData' smart constructor.
data MessageData = MessageData'
  { -- | The message text.
    value :: Prelude.Maybe Prelude.Text,
    -- | The error code or status code associated with the message.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'messageData_value' - The message text.
--
-- 'code', 'messageData_code' - The error code or status code associated with the message.
newMessageData ::
  MessageData
newMessageData =
  MessageData'
    { value = Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The message text.
messageData_value :: Lens.Lens' MessageData (Prelude.Maybe Prelude.Text)
messageData_value = Lens.lens (\MessageData' {value} -> value) (\s@MessageData' {} a -> s {value = a} :: MessageData)

-- | The error code or status code associated with the message.
messageData_code :: Lens.Lens' MessageData (Prelude.Maybe Prelude.Text)
messageData_code = Lens.lens (\MessageData' {code} -> code) (\s@MessageData' {} a -> s {code = a} :: MessageData)

instance Core.FromXML MessageData where
  parseXML x =
    MessageData'
      Prelude.<$> (x Core..@? "Value") Prelude.<*> (x Core..@? "Code")

instance Prelude.Hashable MessageData

instance Prelude.NFData MessageData
