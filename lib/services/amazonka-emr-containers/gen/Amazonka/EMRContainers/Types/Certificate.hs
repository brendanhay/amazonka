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
-- Module      : Amazonka.EMRContainers.Types.Certificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.Certificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The entity representing certificate data generated for managed endpoint.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The ARN of the certificate generated for managed endpoint.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The base64 encoded PEM certificate data generated for managed endpoint.
    certificateData :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'certificate_certificateArn' - The ARN of the certificate generated for managed endpoint.
--
-- 'certificateData', 'certificate_certificateData' - The base64 encoded PEM certificate data generated for managed endpoint.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { certificateArn = Prelude.Nothing,
      certificateData = Prelude.Nothing
    }

-- | The ARN of the certificate generated for managed endpoint.
certificate_certificateArn :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

-- | The base64 encoded PEM certificate data generated for managed endpoint.
certificate_certificateData :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateData = Lens.lens (\Certificate' {certificateData} -> certificateData) (\s@Certificate' {} a -> s {certificateData = a} :: Certificate)

instance Data.FromJSON Certificate where
  parseJSON =
    Data.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Prelude.<$> (x Data..:? "certificateArn")
            Prelude.<*> (x Data..:? "certificateData")
      )

instance Prelude.Hashable Certificate where
  hashWithSalt _salt Certificate' {..} =
    _salt `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateData

instance Prelude.NFData Certificate where
  rnf Certificate' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateData
