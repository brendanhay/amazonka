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
-- Module      : Amazonka.Connect.Types.InstanceStatusReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.InstanceStatusReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Relevant details why the instance was not successfully created.
--
-- /See:/ 'newInstanceStatusReason' smart constructor.
data InstanceStatusReason = InstanceStatusReason'
  { -- | The message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceStatusReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'instanceStatusReason_message' - The message.
newInstanceStatusReason ::
  InstanceStatusReason
newInstanceStatusReason =
  InstanceStatusReason' {message = Prelude.Nothing}

-- | The message.
instanceStatusReason_message :: Lens.Lens' InstanceStatusReason (Prelude.Maybe Prelude.Text)
instanceStatusReason_message = Lens.lens (\InstanceStatusReason' {message} -> message) (\s@InstanceStatusReason' {} a -> s {message = a} :: InstanceStatusReason)

instance Data.FromJSON InstanceStatusReason where
  parseJSON =
    Data.withObject
      "InstanceStatusReason"
      ( \x ->
          InstanceStatusReason'
            Prelude.<$> (x Data..:? "Message")
      )

instance Prelude.Hashable InstanceStatusReason where
  hashWithSalt _salt InstanceStatusReason' {..} =
    _salt `Prelude.hashWithSalt` message

instance Prelude.NFData InstanceStatusReason where
  rnf InstanceStatusReason' {..} = Prelude.rnf message
