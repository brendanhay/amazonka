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
-- Module      : Amazonka.IoT.Types.ImplicitDeny
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ImplicitDeny where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.Policy
import qualified Amazonka.Prelude as Prelude

-- | Information that implicitly denies authorization. When policy doesn\'t
-- explicitly deny or allow an action on a resource it is considered an
-- implicit deny.
--
-- /See:/ 'newImplicitDeny' smart constructor.
data ImplicitDeny = ImplicitDeny'
  { -- | Policies that don\'t contain a matching allow or deny statement for the
    -- specified action on the specified resource.
    policies :: Prelude.Maybe [Policy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImplicitDeny' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'implicitDeny_policies' - Policies that don\'t contain a matching allow or deny statement for the
-- specified action on the specified resource.
newImplicitDeny ::
  ImplicitDeny
newImplicitDeny =
  ImplicitDeny' {policies = Prelude.Nothing}

-- | Policies that don\'t contain a matching allow or deny statement for the
-- specified action on the specified resource.
implicitDeny_policies :: Lens.Lens' ImplicitDeny (Prelude.Maybe [Policy])
implicitDeny_policies = Lens.lens (\ImplicitDeny' {policies} -> policies) (\s@ImplicitDeny' {} a -> s {policies = a} :: ImplicitDeny) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ImplicitDeny where
  parseJSON =
    Data.withObject
      "ImplicitDeny"
      ( \x ->
          ImplicitDeny'
            Prelude.<$> (x Data..:? "policies" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ImplicitDeny where
  hashWithSalt _salt ImplicitDeny' {..} =
    _salt `Prelude.hashWithSalt` policies

instance Prelude.NFData ImplicitDeny where
  rnf ImplicitDeny' {..} = Prelude.rnf policies
