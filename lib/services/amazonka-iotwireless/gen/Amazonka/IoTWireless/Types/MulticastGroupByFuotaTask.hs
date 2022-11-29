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
-- Module      : Amazonka.IoTWireless.Types.MulticastGroupByFuotaTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.MulticastGroupByFuotaTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A multicast group that is associated with a FUOTA task.
--
-- /See:/ 'newMulticastGroupByFuotaTask' smart constructor.
data MulticastGroupByFuotaTask = MulticastGroupByFuotaTask'
  { id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MulticastGroupByFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'multicastGroupByFuotaTask_id' - Undocumented member.
newMulticastGroupByFuotaTask ::
  MulticastGroupByFuotaTask
newMulticastGroupByFuotaTask =
  MulticastGroupByFuotaTask' {id = Prelude.Nothing}

-- | Undocumented member.
multicastGroupByFuotaTask_id :: Lens.Lens' MulticastGroupByFuotaTask (Prelude.Maybe Prelude.Text)
multicastGroupByFuotaTask_id = Lens.lens (\MulticastGroupByFuotaTask' {id} -> id) (\s@MulticastGroupByFuotaTask' {} a -> s {id = a} :: MulticastGroupByFuotaTask)

instance Core.FromJSON MulticastGroupByFuotaTask where
  parseJSON =
    Core.withObject
      "MulticastGroupByFuotaTask"
      ( \x ->
          MulticastGroupByFuotaTask'
            Prelude.<$> (x Core..:? "Id")
      )

instance Prelude.Hashable MulticastGroupByFuotaTask where
  hashWithSalt _salt MulticastGroupByFuotaTask' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData MulticastGroupByFuotaTask where
  rnf MulticastGroupByFuotaTask' {..} = Prelude.rnf id
