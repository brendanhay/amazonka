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
-- Module      : Network.AWS.IoT.Types.Denied
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Denied where

import Network.AWS.IoT.Types.ExplicitDeny
import Network.AWS.IoT.Types.ImplicitDeny
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information that denied the authorization.
--
-- /See:/ 'newDenied' smart constructor.
data Denied = Denied'
  { -- | Information that implicitly denies the authorization. When a policy
    -- doesn\'t explicitly deny or allow an action on a resource it is
    -- considered an implicit deny.
    implicitDeny :: Prelude.Maybe ImplicitDeny,
    -- | Information that explicitly denies the authorization.
    explicitDeny :: Prelude.Maybe ExplicitDeny
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Denied' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'implicitDeny', 'denied_implicitDeny' - Information that implicitly denies the authorization. When a policy
-- doesn\'t explicitly deny or allow an action on a resource it is
-- considered an implicit deny.
--
-- 'explicitDeny', 'denied_explicitDeny' - Information that explicitly denies the authorization.
newDenied ::
  Denied
newDenied =
  Denied'
    { implicitDeny = Prelude.Nothing,
      explicitDeny = Prelude.Nothing
    }

-- | Information that implicitly denies the authorization. When a policy
-- doesn\'t explicitly deny or allow an action on a resource it is
-- considered an implicit deny.
denied_implicitDeny :: Lens.Lens' Denied (Prelude.Maybe ImplicitDeny)
denied_implicitDeny = Lens.lens (\Denied' {implicitDeny} -> implicitDeny) (\s@Denied' {} a -> s {implicitDeny = a} :: Denied)

-- | Information that explicitly denies the authorization.
denied_explicitDeny :: Lens.Lens' Denied (Prelude.Maybe ExplicitDeny)
denied_explicitDeny = Lens.lens (\Denied' {explicitDeny} -> explicitDeny) (\s@Denied' {} a -> s {explicitDeny = a} :: Denied)

instance Prelude.FromJSON Denied where
  parseJSON =
    Prelude.withObject
      "Denied"
      ( \x ->
          Denied'
            Prelude.<$> (x Prelude..:? "implicitDeny")
            Prelude.<*> (x Prelude..:? "explicitDeny")
      )

instance Prelude.Hashable Denied

instance Prelude.NFData Denied
