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
-- Module      : Amazonka.Connect.Types.RoutingProfileSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.RoutingProfileSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a routing profile.
--
-- /See:/ 'newRoutingProfileSummary' smart constructor.
data RoutingProfileSummary = RoutingProfileSummary'
  { -- | The Amazon Resource Name (ARN) of the routing profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the routing profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the routing profile.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingProfileSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'routingProfileSummary_arn' - The Amazon Resource Name (ARN) of the routing profile.
--
-- 'id', 'routingProfileSummary_id' - The identifier of the routing profile.
--
-- 'name', 'routingProfileSummary_name' - The name of the routing profile.
newRoutingProfileSummary ::
  RoutingProfileSummary
newRoutingProfileSummary =
  RoutingProfileSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the routing profile.
routingProfileSummary_arn :: Lens.Lens' RoutingProfileSummary (Prelude.Maybe Prelude.Text)
routingProfileSummary_arn = Lens.lens (\RoutingProfileSummary' {arn} -> arn) (\s@RoutingProfileSummary' {} a -> s {arn = a} :: RoutingProfileSummary)

-- | The identifier of the routing profile.
routingProfileSummary_id :: Lens.Lens' RoutingProfileSummary (Prelude.Maybe Prelude.Text)
routingProfileSummary_id = Lens.lens (\RoutingProfileSummary' {id} -> id) (\s@RoutingProfileSummary' {} a -> s {id = a} :: RoutingProfileSummary)

-- | The name of the routing profile.
routingProfileSummary_name :: Lens.Lens' RoutingProfileSummary (Prelude.Maybe Prelude.Text)
routingProfileSummary_name = Lens.lens (\RoutingProfileSummary' {name} -> name) (\s@RoutingProfileSummary' {} a -> s {name = a} :: RoutingProfileSummary)

instance Data.FromJSON RoutingProfileSummary where
  parseJSON =
    Data.withObject
      "RoutingProfileSummary"
      ( \x ->
          RoutingProfileSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable RoutingProfileSummary where
  hashWithSalt _salt RoutingProfileSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData RoutingProfileSummary where
  rnf RoutingProfileSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
