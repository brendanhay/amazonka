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
-- Module      : Amazonka.CloudWatch.Types.MessageData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.MessageData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { -- | The error code or status code associated with the message.
    code :: Prelude.Maybe Prelude.Text,
    -- | The message text.
    value :: Prelude.Maybe Prelude.Text
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

instance Data.FromXML MessageData where
  parseXML x =
    MessageData'
      Prelude.<$> (x Data..@? "Code") Prelude.<*> (x Data..@? "Value")

instance Prelude.Hashable MessageData where
  hashWithSalt _salt MessageData' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` value

instance Prelude.NFData MessageData where
  rnf MessageData' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf value
