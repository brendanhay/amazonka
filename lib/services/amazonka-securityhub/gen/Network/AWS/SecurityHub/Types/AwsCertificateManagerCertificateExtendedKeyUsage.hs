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
-- Module      : Network.AWS.SecurityHub.Types.AwsCertificateManagerCertificateExtendedKeyUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsCertificateManagerCertificateExtendedKeyUsage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an extended key usage X.509 v3 extension
-- object.
--
-- /See:/ 'newAwsCertificateManagerCertificateExtendedKeyUsage' smart constructor.
data AwsCertificateManagerCertificateExtendedKeyUsage = AwsCertificateManagerCertificateExtendedKeyUsage'
  { -- | An object identifier (OID) for the extension value.
    --
    -- The format is numbers separated by periods.
    oId :: Prelude.Maybe Prelude.Text,
    -- | The name of an extension value. Indicates the purpose for which the
    -- certificate public key can be used.
    name :: Prelude.Maybe Prelude.Text
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
-- 'oId', 'awsCertificateManagerCertificateExtendedKeyUsage_oId' - An object identifier (OID) for the extension value.
--
-- The format is numbers separated by periods.
--
-- 'name', 'awsCertificateManagerCertificateExtendedKeyUsage_name' - The name of an extension value. Indicates the purpose for which the
-- certificate public key can be used.
newAwsCertificateManagerCertificateExtendedKeyUsage ::
  AwsCertificateManagerCertificateExtendedKeyUsage
newAwsCertificateManagerCertificateExtendedKeyUsage =
  AwsCertificateManagerCertificateExtendedKeyUsage'
    { oId =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | An object identifier (OID) for the extension value.
--
-- The format is numbers separated by periods.
awsCertificateManagerCertificateExtendedKeyUsage_oId :: Lens.Lens' AwsCertificateManagerCertificateExtendedKeyUsage (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateExtendedKeyUsage_oId = Lens.lens (\AwsCertificateManagerCertificateExtendedKeyUsage' {oId} -> oId) (\s@AwsCertificateManagerCertificateExtendedKeyUsage' {} a -> s {oId = a} :: AwsCertificateManagerCertificateExtendedKeyUsage)

-- | The name of an extension value. Indicates the purpose for which the
-- certificate public key can be used.
awsCertificateManagerCertificateExtendedKeyUsage_name :: Lens.Lens' AwsCertificateManagerCertificateExtendedKeyUsage (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateExtendedKeyUsage_name = Lens.lens (\AwsCertificateManagerCertificateExtendedKeyUsage' {name} -> name) (\s@AwsCertificateManagerCertificateExtendedKeyUsage' {} a -> s {name = a} :: AwsCertificateManagerCertificateExtendedKeyUsage)

instance
  Core.FromJSON
    AwsCertificateManagerCertificateExtendedKeyUsage
  where
  parseJSON =
    Core.withObject
      "AwsCertificateManagerCertificateExtendedKeyUsage"
      ( \x ->
          AwsCertificateManagerCertificateExtendedKeyUsage'
            Prelude.<$> (x Core..:? "OId") Prelude.<*> (x Core..:? "Name")
      )

instance
  Prelude.Hashable
    AwsCertificateManagerCertificateExtendedKeyUsage

instance
  Prelude.NFData
    AwsCertificateManagerCertificateExtendedKeyUsage

instance
  Core.ToJSON
    AwsCertificateManagerCertificateExtendedKeyUsage
  where
  toJSON
    AwsCertificateManagerCertificateExtendedKeyUsage' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("OId" Core..=) Prelude.<$> oId,
              ("Name" Core..=) Prelude.<$> name
            ]
        )
