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
-- Module      : Amazonka.IoTWireless.Types.FuotaTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.FuotaTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A FUOTA task.
--
-- /See:/ 'newFuotaTask' smart constructor.
data FuotaTask = FuotaTask'
  { name :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'fuotaTask_name' - Undocumented member.
--
-- 'arn', 'fuotaTask_arn' - Undocumented member.
--
-- 'id', 'fuotaTask_id' - Undocumented member.
newFuotaTask ::
  FuotaTask
newFuotaTask =
  FuotaTask'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Undocumented member.
fuotaTask_name :: Lens.Lens' FuotaTask (Prelude.Maybe Prelude.Text)
fuotaTask_name = Lens.lens (\FuotaTask' {name} -> name) (\s@FuotaTask' {} a -> s {name = a} :: FuotaTask)

-- | Undocumented member.
fuotaTask_arn :: Lens.Lens' FuotaTask (Prelude.Maybe Prelude.Text)
fuotaTask_arn = Lens.lens (\FuotaTask' {arn} -> arn) (\s@FuotaTask' {} a -> s {arn = a} :: FuotaTask)

-- | Undocumented member.
fuotaTask_id :: Lens.Lens' FuotaTask (Prelude.Maybe Prelude.Text)
fuotaTask_id = Lens.lens (\FuotaTask' {id} -> id) (\s@FuotaTask' {} a -> s {id = a} :: FuotaTask)

instance Core.FromJSON FuotaTask where
  parseJSON =
    Core.withObject
      "FuotaTask"
      ( \x ->
          FuotaTask'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable FuotaTask where
  hashWithSalt _salt FuotaTask' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData FuotaTask where
  rnf FuotaTask' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
