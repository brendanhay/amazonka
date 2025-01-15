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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types.IdentityPoolShortDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A description of the identity pool.
--
-- /See:/ 'newIdentityPoolShortDescription' smart constructor.
data IdentityPoolShortDescription = IdentityPoolShortDescription'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | A string that you provide.
    identityPoolName :: Prelude.Maybe Prelude.Text
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
-- 'identityPoolId', 'identityPoolShortDescription_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'identityPoolName', 'identityPoolShortDescription_identityPoolName' - A string that you provide.
newIdentityPoolShortDescription ::
  IdentityPoolShortDescription
newIdentityPoolShortDescription =
  IdentityPoolShortDescription'
    { identityPoolId =
        Prelude.Nothing,
      identityPoolName = Prelude.Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
identityPoolShortDescription_identityPoolId :: Lens.Lens' IdentityPoolShortDescription (Prelude.Maybe Prelude.Text)
identityPoolShortDescription_identityPoolId = Lens.lens (\IdentityPoolShortDescription' {identityPoolId} -> identityPoolId) (\s@IdentityPoolShortDescription' {} a -> s {identityPoolId = a} :: IdentityPoolShortDescription)

-- | A string that you provide.
identityPoolShortDescription_identityPoolName :: Lens.Lens' IdentityPoolShortDescription (Prelude.Maybe Prelude.Text)
identityPoolShortDescription_identityPoolName = Lens.lens (\IdentityPoolShortDescription' {identityPoolName} -> identityPoolName) (\s@IdentityPoolShortDescription' {} a -> s {identityPoolName = a} :: IdentityPoolShortDescription)

instance Data.FromJSON IdentityPoolShortDescription where
  parseJSON =
    Data.withObject
      "IdentityPoolShortDescription"
      ( \x ->
          IdentityPoolShortDescription'
            Prelude.<$> (x Data..:? "IdentityPoolId")
            Prelude.<*> (x Data..:? "IdentityPoolName")
      )

instance
  Prelude.Hashable
    IdentityPoolShortDescription
  where
  hashWithSalt _salt IdentityPoolShortDescription' {..} =
    _salt
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` identityPoolName

instance Prelude.NFData IdentityPoolShortDescription where
  rnf IdentityPoolShortDescription' {..} =
    Prelude.rnf identityPoolId `Prelude.seq`
      Prelude.rnf identityPoolName
