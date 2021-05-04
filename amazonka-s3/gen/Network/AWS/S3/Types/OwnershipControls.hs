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
-- Module      : Network.AWS.S3.Types.OwnershipControls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OwnershipControls where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.OwnershipControlsRule

-- | The container element for a bucket\'s ownership controls.
--
-- /See:/ 'newOwnershipControls' smart constructor.
data OwnershipControls = OwnershipControls'
  { -- | The container element for an ownership control rule.
    rules :: [OwnershipControlsRule]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OwnershipControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'ownershipControls_rules' - The container element for an ownership control rule.
newOwnershipControls ::
  OwnershipControls
newOwnershipControls =
  OwnershipControls' {rules = Prelude.mempty}

-- | The container element for an ownership control rule.
ownershipControls_rules :: Lens.Lens' OwnershipControls [OwnershipControlsRule]
ownershipControls_rules = Lens.lens (\OwnershipControls' {rules} -> rules) (\s@OwnershipControls' {} a -> s {rules = a} :: OwnershipControls) Prelude.. Prelude._Coerce

instance Prelude.FromXML OwnershipControls where
  parseXML x =
    OwnershipControls'
      Prelude.<$> (Prelude.parseXMLList "Rule" x)

instance Prelude.Hashable OwnershipControls

instance Prelude.NFData OwnershipControls

instance Prelude.ToXML OwnershipControls where
  toXML OwnershipControls' {..} =
    Prelude.mconcat [Prelude.toXMLList "Rule" rules]
