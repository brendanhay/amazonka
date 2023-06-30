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
-- Module      : Amazonka.IoT.Types.Denied
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Denied where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ExplicitDeny
import Amazonka.IoT.Types.ImplicitDeny
import qualified Amazonka.Prelude as Prelude

-- | Contains information that denied the authorization.
--
-- /See:/ 'newDenied' smart constructor.
data Denied = Denied'
  { -- | Information that explicitly denies the authorization.
    explicitDeny :: Prelude.Maybe ExplicitDeny,
    -- | Information that implicitly denies the authorization. When a policy
    -- doesn\'t explicitly deny or allow an action on a resource it is
    -- considered an implicit deny.
    implicitDeny :: Prelude.Maybe ImplicitDeny
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Denied' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'explicitDeny', 'denied_explicitDeny' - Information that explicitly denies the authorization.
--
-- 'implicitDeny', 'denied_implicitDeny' - Information that implicitly denies the authorization. When a policy
-- doesn\'t explicitly deny or allow an action on a resource it is
-- considered an implicit deny.
newDenied ::
  Denied
newDenied =
  Denied'
    { explicitDeny = Prelude.Nothing,
      implicitDeny = Prelude.Nothing
    }

-- | Information that explicitly denies the authorization.
denied_explicitDeny :: Lens.Lens' Denied (Prelude.Maybe ExplicitDeny)
denied_explicitDeny = Lens.lens (\Denied' {explicitDeny} -> explicitDeny) (\s@Denied' {} a -> s {explicitDeny = a} :: Denied)

-- | Information that implicitly denies the authorization. When a policy
-- doesn\'t explicitly deny or allow an action on a resource it is
-- considered an implicit deny.
denied_implicitDeny :: Lens.Lens' Denied (Prelude.Maybe ImplicitDeny)
denied_implicitDeny = Lens.lens (\Denied' {implicitDeny} -> implicitDeny) (\s@Denied' {} a -> s {implicitDeny = a} :: Denied)

instance Data.FromJSON Denied where
  parseJSON =
    Data.withObject
      "Denied"
      ( \x ->
          Denied'
            Prelude.<$> (x Data..:? "explicitDeny")
            Prelude.<*> (x Data..:? "implicitDeny")
      )

instance Prelude.Hashable Denied where
  hashWithSalt _salt Denied' {..} =
    _salt
      `Prelude.hashWithSalt` explicitDeny
      `Prelude.hashWithSalt` implicitDeny

instance Prelude.NFData Denied where
  rnf Denied' {..} =
    Prelude.rnf explicitDeny
      `Prelude.seq` Prelude.rnf implicitDeny
