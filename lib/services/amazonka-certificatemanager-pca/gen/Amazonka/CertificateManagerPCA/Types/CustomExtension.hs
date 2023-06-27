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
-- Module      : Amazonka.CertificateManagerPCA.Types.CustomExtension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.CustomExtension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the X.509 extension information for a certificate.
--
-- Extensions present in @CustomExtensions@ follow the @ApiPassthrough@
-- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html#template-order-of-operations template rules>.
--
-- /See:/ 'newCustomExtension' smart constructor.
data CustomExtension = CustomExtension'
  { -- | Specifies the critical flag of the X.509 extension.
    critical :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the object identifier (OID) of the X.509 extension. For more
    -- information, see the
    -- <https://oidref.com/2.5.29 Global OID reference database.>
    objectIdentifier :: Prelude.Text,
    -- | Specifies the base64-encoded value of the X.509 extension.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomExtension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'critical', 'customExtension_critical' - Specifies the critical flag of the X.509 extension.
--
-- 'objectIdentifier', 'customExtension_objectIdentifier' - Specifies the object identifier (OID) of the X.509 extension. For more
-- information, see the
-- <https://oidref.com/2.5.29 Global OID reference database.>
--
-- 'value', 'customExtension_value' - Specifies the base64-encoded value of the X.509 extension.
newCustomExtension ::
  -- | 'objectIdentifier'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  CustomExtension
newCustomExtension pObjectIdentifier_ pValue_ =
  CustomExtension'
    { critical = Prelude.Nothing,
      objectIdentifier = pObjectIdentifier_,
      value = pValue_
    }

-- | Specifies the critical flag of the X.509 extension.
customExtension_critical :: Lens.Lens' CustomExtension (Prelude.Maybe Prelude.Bool)
customExtension_critical = Lens.lens (\CustomExtension' {critical} -> critical) (\s@CustomExtension' {} a -> s {critical = a} :: CustomExtension)

-- | Specifies the object identifier (OID) of the X.509 extension. For more
-- information, see the
-- <https://oidref.com/2.5.29 Global OID reference database.>
customExtension_objectIdentifier :: Lens.Lens' CustomExtension Prelude.Text
customExtension_objectIdentifier = Lens.lens (\CustomExtension' {objectIdentifier} -> objectIdentifier) (\s@CustomExtension' {} a -> s {objectIdentifier = a} :: CustomExtension)

-- | Specifies the base64-encoded value of the X.509 extension.
customExtension_value :: Lens.Lens' CustomExtension Prelude.Text
customExtension_value = Lens.lens (\CustomExtension' {value} -> value) (\s@CustomExtension' {} a -> s {value = a} :: CustomExtension)

instance Prelude.Hashable CustomExtension where
  hashWithSalt _salt CustomExtension' {..} =
    _salt
      `Prelude.hashWithSalt` critical
      `Prelude.hashWithSalt` objectIdentifier
      `Prelude.hashWithSalt` value

instance Prelude.NFData CustomExtension where
  rnf CustomExtension' {..} =
    Prelude.rnf critical
      `Prelude.seq` Prelude.rnf objectIdentifier
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON CustomExtension where
  toJSON CustomExtension' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Critical" Data..=) Prelude.<$> critical,
            Prelude.Just
              ("ObjectIdentifier" Data..= objectIdentifier),
            Prelude.Just ("Value" Data..= value)
          ]
      )
