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
-- Module      : Network.AWS.GuardDuty.Types.Owner
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Owner where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information on the owner of the bucket.
--
-- /See:/ 'newOwner' smart constructor.
data Owner = Owner'
  { -- | The canonical user ID of the bucket owner. For information about
    -- locating your canonical user ID see
    -- <https://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html#FindingCanonicalId Finding Your Account Canonical User ID.>
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Owner' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'owner_id' - The canonical user ID of the bucket owner. For information about
-- locating your canonical user ID see
-- <https://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html#FindingCanonicalId Finding Your Account Canonical User ID.>
newOwner ::
  Owner
newOwner = Owner' {id = Prelude.Nothing}

-- | The canonical user ID of the bucket owner. For information about
-- locating your canonical user ID see
-- <https://docs.aws.amazon.com/general/latest/gr/acct-identifiers.html#FindingCanonicalId Finding Your Account Canonical User ID.>
owner_id :: Lens.Lens' Owner (Prelude.Maybe Prelude.Text)
owner_id = Lens.lens (\Owner' {id} -> id) (\s@Owner' {} a -> s {id = a} :: Owner)

instance Prelude.FromJSON Owner where
  parseJSON =
    Prelude.withObject
      "Owner"
      (\x -> Owner' Prelude.<$> (x Prelude..:? "id"))

instance Prelude.Hashable Owner

instance Prelude.NFData Owner
