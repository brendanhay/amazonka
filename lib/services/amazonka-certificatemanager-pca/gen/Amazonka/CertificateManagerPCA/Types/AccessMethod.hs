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
-- Module      : Amazonka.CertificateManagerPCA.Types.AccessMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.AccessMethod where

import Amazonka.CertificateManagerPCA.Types.AccessMethodType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the type and format of extension access. Only one of
-- @CustomObjectIdentifier@ or @AccessMethodType@ may be provided.
-- Providing both results in @InvalidArgsException@.
--
-- /See:/ 'newAccessMethod' smart constructor.
data AccessMethod = AccessMethod'
  { -- | Specifies the @AccessMethod@.
    accessMethodType :: Prelude.Maybe AccessMethodType,
    -- | An object identifier (OID) specifying the @AccessMethod@. The OID must
    -- satisfy the regular expression shown below. For more information, see
    -- NIST\'s definition of
    -- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
    customObjectIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessMethodType', 'accessMethod_accessMethodType' - Specifies the @AccessMethod@.
--
-- 'customObjectIdentifier', 'accessMethod_customObjectIdentifier' - An object identifier (OID) specifying the @AccessMethod@. The OID must
-- satisfy the regular expression shown below. For more information, see
-- NIST\'s definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
newAccessMethod ::
  AccessMethod
newAccessMethod =
  AccessMethod'
    { accessMethodType = Prelude.Nothing,
      customObjectIdentifier = Prelude.Nothing
    }

-- | Specifies the @AccessMethod@.
accessMethod_accessMethodType :: Lens.Lens' AccessMethod (Prelude.Maybe AccessMethodType)
accessMethod_accessMethodType = Lens.lens (\AccessMethod' {accessMethodType} -> accessMethodType) (\s@AccessMethod' {} a -> s {accessMethodType = a} :: AccessMethod)

-- | An object identifier (OID) specifying the @AccessMethod@. The OID must
-- satisfy the regular expression shown below. For more information, see
-- NIST\'s definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
accessMethod_customObjectIdentifier :: Lens.Lens' AccessMethod (Prelude.Maybe Prelude.Text)
accessMethod_customObjectIdentifier = Lens.lens (\AccessMethod' {customObjectIdentifier} -> customObjectIdentifier) (\s@AccessMethod' {} a -> s {customObjectIdentifier = a} :: AccessMethod)

instance Data.FromJSON AccessMethod where
  parseJSON =
    Data.withObject
      "AccessMethod"
      ( \x ->
          AccessMethod'
            Prelude.<$> (x Data..:? "AccessMethodType")
            Prelude.<*> (x Data..:? "CustomObjectIdentifier")
      )

instance Prelude.Hashable AccessMethod where
  hashWithSalt _salt AccessMethod' {..} =
    _salt
      `Prelude.hashWithSalt` accessMethodType
      `Prelude.hashWithSalt` customObjectIdentifier

instance Prelude.NFData AccessMethod where
  rnf AccessMethod' {..} =
    Prelude.rnf accessMethodType `Prelude.seq`
      Prelude.rnf customObjectIdentifier

instance Data.ToJSON AccessMethod where
  toJSON AccessMethod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessMethodType" Data..=)
              Prelude.<$> accessMethodType,
            ("CustomObjectIdentifier" Data..=)
              Prelude.<$> customObjectIdentifier
          ]
      )
