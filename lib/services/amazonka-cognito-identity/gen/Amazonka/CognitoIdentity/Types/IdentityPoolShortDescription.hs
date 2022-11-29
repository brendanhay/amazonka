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
-- Module      : Amazonka.CognitoIdentity.Types.IdentityPoolShortDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types.IdentityPoolShortDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A description of the identity pool.
--
-- /See:/ 'newIdentityPoolShortDescription' smart constructor.
data IdentityPoolShortDescription = IdentityPoolShortDescription'
  { -- | A string that you provide.
    identityPoolName :: Prelude.Maybe Prelude.Text,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityPoolShortDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolName', 'identityPoolShortDescription_identityPoolName' - A string that you provide.
--
-- 'identityPoolId', 'identityPoolShortDescription_identityPoolId' - An identity pool ID in the format REGION:GUID.
newIdentityPoolShortDescription ::
  IdentityPoolShortDescription
newIdentityPoolShortDescription =
  IdentityPoolShortDescription'
    { identityPoolName =
        Prelude.Nothing,
      identityPoolId = Prelude.Nothing
    }

-- | A string that you provide.
identityPoolShortDescription_identityPoolName :: Lens.Lens' IdentityPoolShortDescription (Prelude.Maybe Prelude.Text)
identityPoolShortDescription_identityPoolName = Lens.lens (\IdentityPoolShortDescription' {identityPoolName} -> identityPoolName) (\s@IdentityPoolShortDescription' {} a -> s {identityPoolName = a} :: IdentityPoolShortDescription)

-- | An identity pool ID in the format REGION:GUID.
identityPoolShortDescription_identityPoolId :: Lens.Lens' IdentityPoolShortDescription (Prelude.Maybe Prelude.Text)
identityPoolShortDescription_identityPoolId = Lens.lens (\IdentityPoolShortDescription' {identityPoolId} -> identityPoolId) (\s@IdentityPoolShortDescription' {} a -> s {identityPoolId = a} :: IdentityPoolShortDescription)

instance Core.FromJSON IdentityPoolShortDescription where
  parseJSON =
    Core.withObject
      "IdentityPoolShortDescription"
      ( \x ->
          IdentityPoolShortDescription'
            Prelude.<$> (x Core..:? "IdentityPoolName")
            Prelude.<*> (x Core..:? "IdentityPoolId")
      )

instance
  Prelude.Hashable
    IdentityPoolShortDescription
  where
  hashWithSalt _salt IdentityPoolShortDescription' {..} =
    _salt `Prelude.hashWithSalt` identityPoolName
      `Prelude.hashWithSalt` identityPoolId

instance Prelude.NFData IdentityPoolShortDescription where
  rnf IdentityPoolShortDescription' {..} =
    Prelude.rnf identityPoolName
      `Prelude.seq` Prelude.rnf identityPoolId
