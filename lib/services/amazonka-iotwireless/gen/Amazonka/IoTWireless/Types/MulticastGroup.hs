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
-- Module      : Amazonka.IoTWireless.Types.MulticastGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.MulticastGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A multicast group.
--
-- /See:/ 'newMulticastGroup' smart constructor.
data MulticastGroup = MulticastGroup'
  { name :: Prelude.Maybe Prelude.Text,
    arn :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'multicastGroup_name' - Undocumented member.
--
-- 'arn', 'multicastGroup_arn' - Undocumented member.
--
-- 'id', 'multicastGroup_id' - Undocumented member.
newMulticastGroup ::
  MulticastGroup
newMulticastGroup =
  MulticastGroup'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | Undocumented member.
multicastGroup_name :: Lens.Lens' MulticastGroup (Prelude.Maybe Prelude.Text)
multicastGroup_name = Lens.lens (\MulticastGroup' {name} -> name) (\s@MulticastGroup' {} a -> s {name = a} :: MulticastGroup)

-- | Undocumented member.
multicastGroup_arn :: Lens.Lens' MulticastGroup (Prelude.Maybe Prelude.Text)
multicastGroup_arn = Lens.lens (\MulticastGroup' {arn} -> arn) (\s@MulticastGroup' {} a -> s {arn = a} :: MulticastGroup)

-- | Undocumented member.
multicastGroup_id :: Lens.Lens' MulticastGroup (Prelude.Maybe Prelude.Text)
multicastGroup_id = Lens.lens (\MulticastGroup' {id} -> id) (\s@MulticastGroup' {} a -> s {id = a} :: MulticastGroup)

instance Core.FromJSON MulticastGroup where
  parseJSON =
    Core.withObject
      "MulticastGroup"
      ( \x ->
          MulticastGroup'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable MulticastGroup where
  hashWithSalt _salt MulticastGroup' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData MulticastGroup where
  rnf MulticastGroup' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
