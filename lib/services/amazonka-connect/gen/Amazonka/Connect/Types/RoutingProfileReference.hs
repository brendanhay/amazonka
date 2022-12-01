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
-- Module      : Amazonka.Connect.Types.RoutingProfileReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RoutingProfileReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the routing profile assigned to the user.
--
-- /See:/ 'newRoutingProfileReference' smart constructor.
data RoutingProfileReference = RoutingProfileReference'
  { -- | The Amazon Resource Name (ARN) of the routing profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the routing profile.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingProfileReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'routingProfileReference_arn' - The Amazon Resource Name (ARN) of the routing profile.
--
-- 'id', 'routingProfileReference_id' - The identifier of the routing profile.
newRoutingProfileReference ::
  RoutingProfileReference
newRoutingProfileReference =
  RoutingProfileReference'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the routing profile.
routingProfileReference_arn :: Lens.Lens' RoutingProfileReference (Prelude.Maybe Prelude.Text)
routingProfileReference_arn = Lens.lens (\RoutingProfileReference' {arn} -> arn) (\s@RoutingProfileReference' {} a -> s {arn = a} :: RoutingProfileReference)

-- | The identifier of the routing profile.
routingProfileReference_id :: Lens.Lens' RoutingProfileReference (Prelude.Maybe Prelude.Text)
routingProfileReference_id = Lens.lens (\RoutingProfileReference' {id} -> id) (\s@RoutingProfileReference' {} a -> s {id = a} :: RoutingProfileReference)

instance Core.FromJSON RoutingProfileReference where
  parseJSON =
    Core.withObject
      "RoutingProfileReference"
      ( \x ->
          RoutingProfileReference'
            Prelude.<$> (x Core..:? "Arn") Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable RoutingProfileReference where
  hashWithSalt _salt RoutingProfileReference' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData RoutingProfileReference where
  rnf RoutingProfileReference' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf id
