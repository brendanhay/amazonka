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
-- Module      : Network.AWS.CloudWatch.Types.MessageData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MessageData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A message returned by the @GetMetricData@API, including a code and a
-- description.
--
-- /See:/ 'newMessageData' smart constructor.
data MessageData = MessageData'
  { -- | The error code or status code associated with the message.
    code :: Prelude.Maybe Prelude.Text,
    -- | The message text.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MessageData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'messageData_code' - The error code or status code associated with the message.
--
-- 'value', 'messageData_value' - The message text.
newMessageData ::
  MessageData
newMessageData =
  MessageData'
    { code = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The error code or status code associated with the message.
messageData_code :: Lens.Lens' MessageData (Prelude.Maybe Prelude.Text)
messageData_code = Lens.lens (\MessageData' {code} -> code) (\s@MessageData' {} a -> s {code = a} :: MessageData)

-- | The message text.
messageData_value :: Lens.Lens' MessageData (Prelude.Maybe Prelude.Text)
messageData_value = Lens.lens (\MessageData' {value} -> value) (\s@MessageData' {} a -> s {value = a} :: MessageData)

instance Prelude.FromXML MessageData where
  parseXML x =
    MessageData'
      Prelude.<$> (x Prelude..@? "Code")
      Prelude.<*> (x Prelude..@? "Value")

instance Prelude.Hashable MessageData

instance Prelude.NFData MessageData
