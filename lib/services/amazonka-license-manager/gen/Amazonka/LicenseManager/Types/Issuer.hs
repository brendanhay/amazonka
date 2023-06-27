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
-- Module      : Amazonka.LicenseManager.Types.Issuer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.Issuer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the issuer of a license.
--
-- /See:/ 'newIssuer' smart constructor.
data Issuer = Issuer'
  { -- | Asymmetric KMS key from Key Management Service. The KMS key must have a
    -- key usage of sign and verify, and support the RSASSA-PSS SHA-256 signing
    -- algorithm.
    signKey :: Prelude.Maybe Prelude.Text,
    -- | Issuer name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Issuer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signKey', 'issuer_signKey' - Asymmetric KMS key from Key Management Service. The KMS key must have a
-- key usage of sign and verify, and support the RSASSA-PSS SHA-256 signing
-- algorithm.
--
-- 'name', 'issuer_name' - Issuer name.
newIssuer ::
  -- | 'name'
  Prelude.Text ->
  Issuer
newIssuer pName_ =
  Issuer' {signKey = Prelude.Nothing, name = pName_}

-- | Asymmetric KMS key from Key Management Service. The KMS key must have a
-- key usage of sign and verify, and support the RSASSA-PSS SHA-256 signing
-- algorithm.
issuer_signKey :: Lens.Lens' Issuer (Prelude.Maybe Prelude.Text)
issuer_signKey = Lens.lens (\Issuer' {signKey} -> signKey) (\s@Issuer' {} a -> s {signKey = a} :: Issuer)

-- | Issuer name.
issuer_name :: Lens.Lens' Issuer Prelude.Text
issuer_name = Lens.lens (\Issuer' {name} -> name) (\s@Issuer' {} a -> s {name = a} :: Issuer)

instance Prelude.Hashable Issuer where
  hashWithSalt _salt Issuer' {..} =
    _salt
      `Prelude.hashWithSalt` signKey
      `Prelude.hashWithSalt` name

instance Prelude.NFData Issuer where
  rnf Issuer' {..} =
    Prelude.rnf signKey `Prelude.seq` Prelude.rnf name

instance Data.ToJSON Issuer where
  toJSON Issuer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SignKey" Data..=) Prelude.<$> signKey,
            Prelude.Just ("Name" Data..= name)
          ]
      )
