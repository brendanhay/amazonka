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
-- Module      : Network.AWS.IoT.Types.ExplicitDeny
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ExplicitDeny where

import Network.AWS.IoT.Types.Policy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information that explicitly denies authorization.
--
-- /See:/ 'newExplicitDeny' smart constructor.
data ExplicitDeny = ExplicitDeny'
  { -- | The policies that denied the authorization.
    policies :: Prelude.Maybe [Policy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExplicitDeny' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'explicitDeny_policies' - The policies that denied the authorization.
newExplicitDeny ::
  ExplicitDeny
newExplicitDeny =
  ExplicitDeny' {policies = Prelude.Nothing}

-- | The policies that denied the authorization.
explicitDeny_policies :: Lens.Lens' ExplicitDeny (Prelude.Maybe [Policy])
explicitDeny_policies = Lens.lens (\ExplicitDeny' {policies} -> policies) (\s@ExplicitDeny' {} a -> s {policies = a} :: ExplicitDeny) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ExplicitDeny where
  parseJSON =
    Prelude.withObject
      "ExplicitDeny"
      ( \x ->
          ExplicitDeny'
            Prelude.<$> ( x Prelude..:? "policies"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ExplicitDeny

instance Prelude.NFData ExplicitDeny
