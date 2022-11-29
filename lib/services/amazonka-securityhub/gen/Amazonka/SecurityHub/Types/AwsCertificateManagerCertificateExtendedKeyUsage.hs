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
-- Module      : Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateExtendedKeyUsage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateExtendedKeyUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an extended key usage X.509 v3 extension
-- object.
--
-- /See:/ 'newAwsCertificateManagerCertificateExtendedKeyUsage' smart constructor.
data AwsCertificateManagerCertificateExtendedKeyUsage = AwsCertificateManagerCertificateExtendedKeyUsage'
  { -- | The name of an extension value. Indicates the purpose for which the
    -- certificate public key can be used.
    name :: Prelude.Maybe Prelude.Text,
    -- | An object identifier (OID) for the extension value.
    --
    -- The format is numbers separated by periods.
    oId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCertificateManagerCertificateExtendedKeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'awsCertificateManagerCertificateExtendedKeyUsage_name' - The name of an extension value. Indicates the purpose for which the
-- certificate public key can be used.
--
-- 'oId', 'awsCertificateManagerCertificateExtendedKeyUsage_oId' - An object identifier (OID) for the extension value.
--
-- The format is numbers separated by periods.
newAwsCertificateManagerCertificateExtendedKeyUsage ::
  AwsCertificateManagerCertificateExtendedKeyUsage
newAwsCertificateManagerCertificateExtendedKeyUsage =
  AwsCertificateManagerCertificateExtendedKeyUsage'
    { name =
        Prelude.Nothing,
      oId = Prelude.Nothing
    }

-- | The name of an extension value. Indicates the purpose for which the
-- certificate public key can be used.
awsCertificateManagerCertificateExtendedKeyUsage_name :: Lens.Lens' AwsCertificateManagerCertificateExtendedKeyUsage (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateExtendedKeyUsage_name = Lens.lens (\AwsCertificateManagerCertificateExtendedKeyUsage' {name} -> name) (\s@AwsCertificateManagerCertificateExtendedKeyUsage' {} a -> s {name = a} :: AwsCertificateManagerCertificateExtendedKeyUsage)

-- | An object identifier (OID) for the extension value.
--
-- The format is numbers separated by periods.
awsCertificateManagerCertificateExtendedKeyUsage_oId :: Lens.Lens' AwsCertificateManagerCertificateExtendedKeyUsage (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateExtendedKeyUsage_oId = Lens.lens (\AwsCertificateManagerCertificateExtendedKeyUsage' {oId} -> oId) (\s@AwsCertificateManagerCertificateExtendedKeyUsage' {} a -> s {oId = a} :: AwsCertificateManagerCertificateExtendedKeyUsage)

instance
  Core.FromJSON
    AwsCertificateManagerCertificateExtendedKeyUsage
  where
  parseJSON =
    Core.withObject
      "AwsCertificateManagerCertificateExtendedKeyUsage"
      ( \x ->
          AwsCertificateManagerCertificateExtendedKeyUsage'
            Prelude.<$> (x Core..:? "Name") Prelude.<*> (x Core..:? "OId")
      )

instance
  Prelude.Hashable
    AwsCertificateManagerCertificateExtendedKeyUsage
  where
  hashWithSalt
    _salt
    AwsCertificateManagerCertificateExtendedKeyUsage' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` oId

instance
  Prelude.NFData
    AwsCertificateManagerCertificateExtendedKeyUsage
  where
  rnf
    AwsCertificateManagerCertificateExtendedKeyUsage' {..} =
      Prelude.rnf name `Prelude.seq` Prelude.rnf oId

instance
  Core.ToJSON
    AwsCertificateManagerCertificateExtendedKeyUsage
  where
  toJSON
    AwsCertificateManagerCertificateExtendedKeyUsage' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Name" Core..=) Prelude.<$> name,
              ("OId" Core..=) Prelude.<$> oId
            ]
        )
