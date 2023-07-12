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
-- Module      : Amazonka.ElasticSearch.Types.AuthorizedPrincipal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.AuthorizedPrincipal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.PrincipalType
import qualified Amazonka.Prelude as Prelude

-- | Information about an account or service that has access to an Amazon
-- OpenSearch Service domain through the use of an interface VPC endpoint.
--
-- /See:/ 'newAuthorizedPrincipal' smart constructor.
data AuthorizedPrincipal = AuthorizedPrincipal'
  { -- | The IAM principal that is allowed access to the domain.
    principal :: Prelude.Maybe Prelude.Text,
    -- | The type of principal.
    principalType :: Prelude.Maybe PrincipalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizedPrincipal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'authorizedPrincipal_principal' - The IAM principal that is allowed access to the domain.
--
-- 'principalType', 'authorizedPrincipal_principalType' - The type of principal.
newAuthorizedPrincipal ::
  AuthorizedPrincipal
newAuthorizedPrincipal =
  AuthorizedPrincipal'
    { principal = Prelude.Nothing,
      principalType = Prelude.Nothing
    }

-- | The IAM principal that is allowed access to the domain.
authorizedPrincipal_principal :: Lens.Lens' AuthorizedPrincipal (Prelude.Maybe Prelude.Text)
authorizedPrincipal_principal = Lens.lens (\AuthorizedPrincipal' {principal} -> principal) (\s@AuthorizedPrincipal' {} a -> s {principal = a} :: AuthorizedPrincipal)

-- | The type of principal.
authorizedPrincipal_principalType :: Lens.Lens' AuthorizedPrincipal (Prelude.Maybe PrincipalType)
authorizedPrincipal_principalType = Lens.lens (\AuthorizedPrincipal' {principalType} -> principalType) (\s@AuthorizedPrincipal' {} a -> s {principalType = a} :: AuthorizedPrincipal)

instance Data.FromJSON AuthorizedPrincipal where
  parseJSON =
    Data.withObject
      "AuthorizedPrincipal"
      ( \x ->
          AuthorizedPrincipal'
            Prelude.<$> (x Data..:? "Principal")
            Prelude.<*> (x Data..:? "PrincipalType")
      )

instance Prelude.Hashable AuthorizedPrincipal where
  hashWithSalt _salt AuthorizedPrincipal' {..} =
    _salt
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` principalType

instance Prelude.NFData AuthorizedPrincipal where
  rnf AuthorizedPrincipal' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf principalType
