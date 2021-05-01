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
-- Module      : Network.AWS.IoT.Types.ImplicitDeny
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ImplicitDeny where

import Network.AWS.IoT.Types.Policy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
implicitDeny_policies = Lens.lens (\ImplicitDeny' {policies} -> policies) (\s@ImplicitDeny' {} a -> s {policies = a} :: ImplicitDeny) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ImplicitDeny where
  parseJSON =
    Prelude.withObject
      "ImplicitDeny"
      ( \x ->
          ImplicitDeny'
            Prelude.<$> ( x Prelude..:? "policies"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ImplicitDeny

instance Prelude.NFData ImplicitDeny
