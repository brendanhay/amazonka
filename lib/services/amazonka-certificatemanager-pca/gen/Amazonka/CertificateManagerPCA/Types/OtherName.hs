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
-- Module      : Amazonka.CertificateManagerPCA.Types.OtherName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.OtherName where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a custom ASN.1 X.400 @GeneralName@ using an object identifier
-- (OID) and value. The OID must satisfy the regular expression shown
-- below. For more information, see NIST\'s definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
--
-- /See:/ 'newOtherName' smart constructor.
data OtherName = OtherName'
  { -- | Specifies an OID.
    typeId :: Prelude.Text,
    -- | Specifies an OID value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OtherName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeId', 'otherName_typeId' - Specifies an OID.
--
-- 'value', 'otherName_value' - Specifies an OID value.
newOtherName ::
  -- | 'typeId'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  OtherName
newOtherName pTypeId_ pValue_ =
  OtherName' {typeId = pTypeId_, value = pValue_}

-- | Specifies an OID.
otherName_typeId :: Lens.Lens' OtherName Prelude.Text
otherName_typeId = Lens.lens (\OtherName' {typeId} -> typeId) (\s@OtherName' {} a -> s {typeId = a} :: OtherName)

-- | Specifies an OID value.
otherName_value :: Lens.Lens' OtherName Prelude.Text
otherName_value = Lens.lens (\OtherName' {value} -> value) (\s@OtherName' {} a -> s {value = a} :: OtherName)

instance Data.FromJSON OtherName where
  parseJSON =
    Data.withObject
      "OtherName"
      ( \x ->
          OtherName'
            Prelude.<$> (x Data..: "TypeId")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable OtherName where
  hashWithSalt _salt OtherName' {..} =
    _salt
      `Prelude.hashWithSalt` typeId
      `Prelude.hashWithSalt` value

instance Prelude.NFData OtherName where
  rnf OtherName' {..} =
    Prelude.rnf typeId `Prelude.seq` Prelude.rnf value

instance Data.ToJSON OtherName where
  toJSON OtherName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TypeId" Data..= typeId),
            Prelude.Just ("Value" Data..= value)
          ]
      )
