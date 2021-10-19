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
-- Module      : Network.AWS.CertificateManagerPCA.Types.ApiPassthrough
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ApiPassthrough where

import Network.AWS.CertificateManagerPCA.Types.ASN1Subject
import Network.AWS.CertificateManagerPCA.Types.Extensions
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains X.509 certificate information to be placed in an issued
-- certificate. An @APIPassthrough@ or @APICSRPassthrough@ template variant
-- must be selected, or else this parameter is ignored.
--
-- If conflicting or duplicate certificate information is supplied from
-- other sources, ACM Private CA applies
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/UsingTemplates.html#template-order-of-operations order of operation rules>
-- to determine what information is used.
--
-- /See:/ 'newApiPassthrough' smart constructor.
data ApiPassthrough = ApiPassthrough'
  { subject :: Prelude.Maybe ASN1Subject,
    -- | Specifies X.509 extension information for a certificate.
    extensions :: Prelude.Maybe Extensions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiPassthrough' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subject', 'apiPassthrough_subject' - Undocumented member.
--
-- 'extensions', 'apiPassthrough_extensions' - Specifies X.509 extension information for a certificate.
newApiPassthrough ::
  ApiPassthrough
newApiPassthrough =
  ApiPassthrough'
    { subject = Prelude.Nothing,
      extensions = Prelude.Nothing
    }

-- | Undocumented member.
apiPassthrough_subject :: Lens.Lens' ApiPassthrough (Prelude.Maybe ASN1Subject)
apiPassthrough_subject = Lens.lens (\ApiPassthrough' {subject} -> subject) (\s@ApiPassthrough' {} a -> s {subject = a} :: ApiPassthrough)

-- | Specifies X.509 extension information for a certificate.
apiPassthrough_extensions :: Lens.Lens' ApiPassthrough (Prelude.Maybe Extensions)
apiPassthrough_extensions = Lens.lens (\ApiPassthrough' {extensions} -> extensions) (\s@ApiPassthrough' {} a -> s {extensions = a} :: ApiPassthrough)

instance Prelude.Hashable ApiPassthrough

instance Prelude.NFData ApiPassthrough

instance Core.ToJSON ApiPassthrough where
  toJSON ApiPassthrough' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Subject" Core..=) Prelude.<$> subject,
            ("Extensions" Core..=) Prelude.<$> extensions
          ]
      )
