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
-- Module      : Network.AWS.ELBv2.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Certificate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an SSL server certificate.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | Indicates whether the certificate is the default certificate. Do not set
    -- this value when specifying a certificate as an input. This value is not
    -- included in the output when describing a listener, but is included when
    -- describing listener certificates.
    isDefault :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefault', 'certificate_isDefault' - Indicates whether the certificate is the default certificate. Do not set
-- this value when specifying a certificate as an input. This value is not
-- included in the output when describing a listener, but is included when
-- describing listener certificates.
--
-- 'certificateArn', 'certificate_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { isDefault = Core.Nothing,
      certificateArn = Core.Nothing
    }

-- | Indicates whether the certificate is the default certificate. Do not set
-- this value when specifying a certificate as an input. This value is not
-- included in the output when describing a listener, but is included when
-- describing listener certificates.
certificate_isDefault :: Lens.Lens' Certificate (Core.Maybe Core.Bool)
certificate_isDefault = Lens.lens (\Certificate' {isDefault} -> isDefault) (\s@Certificate' {} a -> s {isDefault = a} :: Certificate)

-- | The Amazon Resource Name (ARN) of the certificate.
certificate_certificateArn :: Lens.Lens' Certificate (Core.Maybe Core.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

instance Core.FromXML Certificate where
  parseXML x =
    Certificate'
      Core.<$> (x Core..@? "IsDefault")
      Core.<*> (x Core..@? "CertificateArn")

instance Core.Hashable Certificate

instance Core.NFData Certificate

instance Core.ToQuery Certificate where
  toQuery Certificate' {..} =
    Core.mconcat
      [ "IsDefault" Core.=: isDefault,
        "CertificateArn" Core.=: certificateArn
      ]
