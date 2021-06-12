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
-- Module      : Network.AWS.Route53.Types.DelegationSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.DelegationSet where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.Internal

-- | A complex type that lists the name servers in a delegation set, as well
-- as the @CallerReference@ and the @ID@ for the delegation set.
--
-- /See:/ 'newDelegationSet' smart constructor.
data DelegationSet = DelegationSet'
  { -- | The ID that Amazon Route 53 assigns to a reusable delegation set.
    id :: Core.Maybe ResourceId,
    -- | The value that you specified for @CallerReference@ when you created the
    -- reusable delegation set.
    callerReference :: Core.Maybe Core.Text,
    -- | A complex type that contains a list of the authoritative name servers
    -- for a hosted zone or for a reusable delegation set.
    nameServers :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  DelegationSet
newDelegationSet pNameServers_ =
  DelegationSet'
    { id = Core.Nothing,
      callerReference = Core.Nothing,
      nameServers = Lens._Coerce Lens.# pNameServers_
    }

-- | The ID that Amazon Route 53 assigns to a reusable delegation set.
delegationSet_id :: Lens.Lens' DelegationSet (Core.Maybe ResourceId)
delegationSet_id = Lens.lens (\DelegationSet' {id} -> id) (\s@DelegationSet' {} a -> s {id = a} :: DelegationSet)

-- | The value that you specified for @CallerReference@ when you created the
-- reusable delegation set.
delegationSet_callerReference :: Lens.Lens' DelegationSet (Core.Maybe Core.Text)
delegationSet_callerReference = Lens.lens (\DelegationSet' {callerReference} -> callerReference) (\s@DelegationSet' {} a -> s {callerReference = a} :: DelegationSet)

-- | A complex type that contains a list of the authoritative name servers
-- for a hosted zone or for a reusable delegation set.
delegationSet_nameServers :: Lens.Lens' DelegationSet (Core.NonEmpty Core.Text)
delegationSet_nameServers = Lens.lens (\DelegationSet' {nameServers} -> nameServers) (\s@DelegationSet' {} a -> s {nameServers = a} :: DelegationSet) Core.. Lens._Coerce

instance Core.FromXML DelegationSet where
  parseXML x =
    DelegationSet'
      Core.<$> (x Core..@? "Id")
      Core.<*> (x Core..@? "CallerReference")
      Core.<*> ( x Core..@? "NameServers" Core..!@ Core.mempty
                   Core.>>= Core.parseXMLList1 "NameServer"
               )

instance Core.Hashable DelegationSet

instance Core.NFData DelegationSet
