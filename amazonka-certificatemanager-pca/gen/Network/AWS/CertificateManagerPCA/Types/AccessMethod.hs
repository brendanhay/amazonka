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
-- Module      : Network.AWS.CertificateManagerPCA.Types.AccessMethod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.AccessMethod where

import Network.AWS.CertificateManagerPCA.Types.AccessMethodType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AccessMethod where
  parseJSON =
    Prelude.withObject
      "AccessMethod"
      ( \x ->
          AccessMethod'
            Prelude.<$> (x Prelude..:? "AccessMethodType")
            Prelude.<*> (x Prelude..:? "CustomObjectIdentifier")
      )

instance Prelude.Hashable AccessMethod

instance Prelude.NFData AccessMethod

instance Prelude.ToJSON AccessMethod where
  toJSON AccessMethod' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccessMethodType" Prelude..=)
              Prelude.<$> accessMethodType,
            ("CustomObjectIdentifier" Prelude..=)
              Prelude.<$> customObjectIdentifier
          ]
      )
