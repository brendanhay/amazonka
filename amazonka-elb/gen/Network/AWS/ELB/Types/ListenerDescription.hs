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
-- Module      : Network.AWS.ELB.Types.ListenerDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ListenerDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.Listener
import qualified Network.AWS.Lens as Lens

-- | The policies enabled for a listener.
--
-- /See:/ 'newListenerDescription' smart constructor.
data ListenerDescription = ListenerDescription'
  { -- | The listener.
    listener :: Core.Maybe Listener,
    -- | The policies. If there are no policies enabled, the list is empty.
    policyNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListenerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listener', 'listenerDescription_listener' - The listener.
--
-- 'policyNames', 'listenerDescription_policyNames' - The policies. If there are no policies enabled, the list is empty.
newListenerDescription ::
  ListenerDescription
newListenerDescription =
  ListenerDescription'
    { listener = Core.Nothing,
      policyNames = Core.Nothing
    }

-- | The listener.
listenerDescription_listener :: Lens.Lens' ListenerDescription (Core.Maybe Listener)
listenerDescription_listener = Lens.lens (\ListenerDescription' {listener} -> listener) (\s@ListenerDescription' {} a -> s {listener = a} :: ListenerDescription)

-- | The policies. If there are no policies enabled, the list is empty.
listenerDescription_policyNames :: Lens.Lens' ListenerDescription (Core.Maybe [Core.Text])
listenerDescription_policyNames = Lens.lens (\ListenerDescription' {policyNames} -> policyNames) (\s@ListenerDescription' {} a -> s {policyNames = a} :: ListenerDescription) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML ListenerDescription where
  parseXML x =
    ListenerDescription'
      Core.<$> (x Core..@? "Listener")
      Core.<*> ( x Core..@? "PolicyNames" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable ListenerDescription

instance Core.NFData ListenerDescription
