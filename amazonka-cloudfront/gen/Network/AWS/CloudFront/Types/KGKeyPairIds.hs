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
-- Module      : Network.AWS.CloudFront.Types.KGKeyPairIds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KGKeyPairIds where

import Network.AWS.CloudFront.Types.KeyPairIds
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of identifiers for the public keys that CloudFront can use to
-- verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'newKGKeyPairIds' smart constructor.
data KGKeyPairIds = KGKeyPairIds'
  { keyPairIds :: Prelude.Maybe KeyPairIds,
    -- | The identifier of the key group that contains the public keys.
    keyGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KGKeyPairIds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPairIds', 'kGKeyPairIds_keyPairIds' - Undocumented member.
--
-- 'keyGroupId', 'kGKeyPairIds_keyGroupId' - The identifier of the key group that contains the public keys.
newKGKeyPairIds ::
  KGKeyPairIds
newKGKeyPairIds =
  KGKeyPairIds'
    { keyPairIds = Prelude.Nothing,
      keyGroupId = Prelude.Nothing
    }

-- | Undocumented member.
kGKeyPairIds_keyPairIds :: Lens.Lens' KGKeyPairIds (Prelude.Maybe KeyPairIds)
kGKeyPairIds_keyPairIds = Lens.lens (\KGKeyPairIds' {keyPairIds} -> keyPairIds) (\s@KGKeyPairIds' {} a -> s {keyPairIds = a} :: KGKeyPairIds)

-- | The identifier of the key group that contains the public keys.
kGKeyPairIds_keyGroupId :: Lens.Lens' KGKeyPairIds (Prelude.Maybe Prelude.Text)
kGKeyPairIds_keyGroupId = Lens.lens (\KGKeyPairIds' {keyGroupId} -> keyGroupId) (\s@KGKeyPairIds' {} a -> s {keyGroupId = a} :: KGKeyPairIds)

instance Prelude.FromXML KGKeyPairIds where
  parseXML x =
    KGKeyPairIds'
      Prelude.<$> (x Prelude..@? "KeyPairIds")
      Prelude.<*> (x Prelude..@? "KeyGroupId")

instance Prelude.Hashable KGKeyPairIds

instance Prelude.NFData KGKeyPairIds
