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
-- Module      : Amazonka.CertificateManagerPCA.Types.CustomAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.CustomAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the X.500 relative distinguished name (RDN).
--
-- /See:/ 'newCustomAttribute' smart constructor.
data CustomAttribute = CustomAttribute'
  { -- | Specifies the object identifier (OID) of the attribute type of the
    -- relative distinguished name (RDN).
    objectIdentifier :: Prelude.Text,
    -- | Specifies the attribute value of relative distinguished name (RDN).
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectIdentifier', 'customAttribute_objectIdentifier' - Specifies the object identifier (OID) of the attribute type of the
-- relative distinguished name (RDN).
--
-- 'value', 'customAttribute_value' - Specifies the attribute value of relative distinguished name (RDN).
newCustomAttribute ::
  -- | 'objectIdentifier'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  CustomAttribute
newCustomAttribute pObjectIdentifier_ pValue_ =
  CustomAttribute'
    { objectIdentifier =
        pObjectIdentifier_,
      value = pValue_
    }

-- | Specifies the object identifier (OID) of the attribute type of the
-- relative distinguished name (RDN).
customAttribute_objectIdentifier :: Lens.Lens' CustomAttribute Prelude.Text
customAttribute_objectIdentifier = Lens.lens (\CustomAttribute' {objectIdentifier} -> objectIdentifier) (\s@CustomAttribute' {} a -> s {objectIdentifier = a} :: CustomAttribute)

-- | Specifies the attribute value of relative distinguished name (RDN).
customAttribute_value :: Lens.Lens' CustomAttribute Prelude.Text
customAttribute_value = Lens.lens (\CustomAttribute' {value} -> value) (\s@CustomAttribute' {} a -> s {value = a} :: CustomAttribute)

instance Data.FromJSON CustomAttribute where
  parseJSON =
    Data.withObject
      "CustomAttribute"
      ( \x ->
          CustomAttribute'
            Prelude.<$> (x Data..: "ObjectIdentifier")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable CustomAttribute where
  hashWithSalt _salt CustomAttribute' {..} =
    _salt `Prelude.hashWithSalt` objectIdentifier
      `Prelude.hashWithSalt` value

instance Prelude.NFData CustomAttribute where
  rnf CustomAttribute' {..} =
    Prelude.rnf objectIdentifier
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON CustomAttribute where
  toJSON CustomAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectIdentifier" Data..= objectIdentifier),
            Prelude.Just ("Value" Data..= value)
          ]
      )
