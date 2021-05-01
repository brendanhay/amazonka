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
-- Module      : Network.AWS.ELB.Types.ListenerDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ListenerDescription where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.Listener
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The policies enabled for a listener.
--
-- /See:/ 'newListenerDescription' smart constructor.
data ListenerDescription = ListenerDescription'
  { -- | The listener.
    listener :: Prelude.Maybe Listener,
    -- | The policies. If there are no policies enabled, the list is empty.
    policyNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { listener = Prelude.Nothing,
      policyNames = Prelude.Nothing
    }

-- | The listener.
listenerDescription_listener :: Lens.Lens' ListenerDescription (Prelude.Maybe Listener)
listenerDescription_listener = Lens.lens (\ListenerDescription' {listener} -> listener) (\s@ListenerDescription' {} a -> s {listener = a} :: ListenerDescription)

-- | The policies. If there are no policies enabled, the list is empty.
listenerDescription_policyNames :: Lens.Lens' ListenerDescription (Prelude.Maybe [Prelude.Text])
listenerDescription_policyNames = Lens.lens (\ListenerDescription' {policyNames} -> policyNames) (\s@ListenerDescription' {} a -> s {policyNames = a} :: ListenerDescription) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML ListenerDescription where
  parseXML x =
    ListenerDescription'
      Prelude.<$> (x Prelude..@? "Listener")
      Prelude.<*> ( x Prelude..@? "PolicyNames"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable ListenerDescription

instance Prelude.NFData ListenerDescription
