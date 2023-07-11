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
-- Module      : Amazonka.CloudFront.Types.KGKeyPairIds
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.KGKeyPairIds where

import Amazonka.CloudFront.Types.KeyPairIds
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of identifiers for the public keys that CloudFront can use to
-- verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'newKGKeyPairIds' smart constructor.
data KGKeyPairIds = KGKeyPairIds'
  { -- | The identifier of the key group that contains the public keys.
    keyGroupId :: Prelude.Maybe Prelude.Text,
    keyPairIds :: Prelude.Maybe KeyPairIds
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KGKeyPairIds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyGroupId', 'kGKeyPairIds_keyGroupId' - The identifier of the key group that contains the public keys.
--
-- 'keyPairIds', 'kGKeyPairIds_keyPairIds' - Undocumented member.
newKGKeyPairIds ::
  KGKeyPairIds
newKGKeyPairIds =
  KGKeyPairIds'
    { keyGroupId = Prelude.Nothing,
      keyPairIds = Prelude.Nothing
    }

-- | The identifier of the key group that contains the public keys.
kGKeyPairIds_keyGroupId :: Lens.Lens' KGKeyPairIds (Prelude.Maybe Prelude.Text)
kGKeyPairIds_keyGroupId = Lens.lens (\KGKeyPairIds' {keyGroupId} -> keyGroupId) (\s@KGKeyPairIds' {} a -> s {keyGroupId = a} :: KGKeyPairIds)

-- | Undocumented member.
kGKeyPairIds_keyPairIds :: Lens.Lens' KGKeyPairIds (Prelude.Maybe KeyPairIds)
kGKeyPairIds_keyPairIds = Lens.lens (\KGKeyPairIds' {keyPairIds} -> keyPairIds) (\s@KGKeyPairIds' {} a -> s {keyPairIds = a} :: KGKeyPairIds)

instance Data.FromXML KGKeyPairIds where
  parseXML x =
    KGKeyPairIds'
      Prelude.<$> (x Data..@? "KeyGroupId")
      Prelude.<*> (x Data..@? "KeyPairIds")

instance Prelude.Hashable KGKeyPairIds where
  hashWithSalt _salt KGKeyPairIds' {..} =
    _salt
      `Prelude.hashWithSalt` keyGroupId
      `Prelude.hashWithSalt` keyPairIds

instance Prelude.NFData KGKeyPairIds where
  rnf KGKeyPairIds' {..} =
    Prelude.rnf keyGroupId
      `Prelude.seq` Prelude.rnf keyPairIds
