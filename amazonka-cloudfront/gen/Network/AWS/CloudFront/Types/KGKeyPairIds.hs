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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of identifiers for the public keys that CloudFront can use to
-- verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'newKGKeyPairIds' smart constructor.
data KGKeyPairIds = KGKeyPairIds'
  { keyPairIds :: Core.Maybe KeyPairIds,
    -- | The identifier of the key group that contains the public keys.
    keyGroupId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { keyPairIds = Core.Nothing,
      keyGroupId = Core.Nothing
    }

-- | Undocumented member.
kGKeyPairIds_keyPairIds :: Lens.Lens' KGKeyPairIds (Core.Maybe KeyPairIds)
kGKeyPairIds_keyPairIds = Lens.lens (\KGKeyPairIds' {keyPairIds} -> keyPairIds) (\s@KGKeyPairIds' {} a -> s {keyPairIds = a} :: KGKeyPairIds)

-- | The identifier of the key group that contains the public keys.
kGKeyPairIds_keyGroupId :: Lens.Lens' KGKeyPairIds (Core.Maybe Core.Text)
kGKeyPairIds_keyGroupId = Lens.lens (\KGKeyPairIds' {keyGroupId} -> keyGroupId) (\s@KGKeyPairIds' {} a -> s {keyGroupId = a} :: KGKeyPairIds)

instance Core.FromXML KGKeyPairIds where
  parseXML x =
    KGKeyPairIds'
      Core.<$> (x Core..@? "KeyPairIds")
      Core.<*> (x Core..@? "KeyGroupId")

instance Core.Hashable KGKeyPairIds

instance Core.NFData KGKeyPairIds
