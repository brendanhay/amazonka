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
-- Module      : Amazonka.Route53.Types.DelegationSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.DelegationSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that lists the name servers in a delegation set, as well
-- as the @CallerReference@ and the @ID@ for the delegation set.
--
-- /See:/ 'newDelegationSet' smart constructor.
data DelegationSet = DelegationSet'
  { -- | The ID that Amazon Route 53 assigns to a reusable delegation set.
    id :: Prelude.Maybe ResourceId,
    -- | The value that you specified for @CallerReference@ when you created the
    -- reusable delegation set.
    callerReference :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains a list of the authoritative name servers
    -- for a hosted zone or for a reusable delegation set.
    nameServers :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DelegationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'delegationSet_id' - The ID that Amazon Route 53 assigns to a reusable delegation set.
--
-- 'callerReference', 'delegationSet_callerReference' - The value that you specified for @CallerReference@ when you created the
-- reusable delegation set.
--
-- 'nameServers', 'delegationSet_nameServers' - A complex type that contains a list of the authoritative name servers
-- for a hosted zone or for a reusable delegation set.
newDelegationSet ::
  -- | 'nameServers'
  Prelude.NonEmpty Prelude.Text ->
  DelegationSet
newDelegationSet pNameServers_ =
  DelegationSet'
    { id = Prelude.Nothing,
      callerReference = Prelude.Nothing,
      nameServers = Lens.coerced Lens.# pNameServers_
    }

-- | The ID that Amazon Route 53 assigns to a reusable delegation set.
delegationSet_id :: Lens.Lens' DelegationSet (Prelude.Maybe ResourceId)
delegationSet_id = Lens.lens (\DelegationSet' {id} -> id) (\s@DelegationSet' {} a -> s {id = a} :: DelegationSet)

-- | The value that you specified for @CallerReference@ when you created the
-- reusable delegation set.
delegationSet_callerReference :: Lens.Lens' DelegationSet (Prelude.Maybe Prelude.Text)
delegationSet_callerReference = Lens.lens (\DelegationSet' {callerReference} -> callerReference) (\s@DelegationSet' {} a -> s {callerReference = a} :: DelegationSet)

-- | A complex type that contains a list of the authoritative name servers
-- for a hosted zone or for a reusable delegation set.
delegationSet_nameServers :: Lens.Lens' DelegationSet (Prelude.NonEmpty Prelude.Text)
delegationSet_nameServers = Lens.lens (\DelegationSet' {nameServers} -> nameServers) (\s@DelegationSet' {} a -> s {nameServers = a} :: DelegationSet) Prelude.. Lens.coerced

instance Data.FromXML DelegationSet where
  parseXML x =
    DelegationSet'
      Prelude.<$> (x Data..@? "Id")
      Prelude.<*> (x Data..@? "CallerReference")
      Prelude.<*> ( x Data..@? "NameServers" Core..!@ Prelude.mempty
                      Prelude.>>= Data.parseXMLList1 "NameServer"
                  )

instance Prelude.Hashable DelegationSet where
  hashWithSalt _salt DelegationSet' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` callerReference
      `Prelude.hashWithSalt` nameServers

instance Prelude.NFData DelegationSet where
  rnf DelegationSet' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf callerReference
      `Prelude.seq` Prelude.rnf nameServers
