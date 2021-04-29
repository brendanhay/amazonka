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
-- Module      : Network.AWS.CertificateManagerPCA.Types.ApiPassthrough
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ApiPassthrough where

import Network.AWS.CertificateManagerPCA.Types.ASN1Subject
import Network.AWS.CertificateManagerPCA.Types.Extensions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains X.509 certificate information to be placed in an issued
-- certificate. An @APIPassthrough@ or @APICSRPassthrough@ template variant
-- must be selected, or else this parameter is ignored.
--
-- If conflicting or duplicate certificate information is supplied from
-- other sources, ACM Private CA applies <xxxxx order of operation rules>
-- to determine what information is used.
--
-- /See:/ 'newApiPassthrough' smart constructor.
data ApiPassthrough = ApiPassthrough'
  { -- | Specifies X.509 extension information for a certificate.
    extensions :: Prelude.Maybe Extensions,
    subject :: Prelude.Maybe ASN1Subject
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ApiPassthrough' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extensions', 'apiPassthrough_extensions' - Specifies X.509 extension information for a certificate.
--
-- 'subject', 'apiPassthrough_subject' - Undocumented member.
newApiPassthrough ::
  ApiPassthrough
newApiPassthrough =
  ApiPassthrough'
    { extensions = Prelude.Nothing,
      subject = Prelude.Nothing
    }

-- | Specifies X.509 extension information for a certificate.
apiPassthrough_extensions :: Lens.Lens' ApiPassthrough (Prelude.Maybe Extensions)
apiPassthrough_extensions = Lens.lens (\ApiPassthrough' {extensions} -> extensions) (\s@ApiPassthrough' {} a -> s {extensions = a} :: ApiPassthrough)

-- | Undocumented member.
apiPassthrough_subject :: Lens.Lens' ApiPassthrough (Prelude.Maybe ASN1Subject)
apiPassthrough_subject = Lens.lens (\ApiPassthrough' {subject} -> subject) (\s@ApiPassthrough' {} a -> s {subject = a} :: ApiPassthrough)

instance Prelude.Hashable ApiPassthrough

instance Prelude.NFData ApiPassthrough

instance Prelude.ToJSON ApiPassthrough where
  toJSON ApiPassthrough' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Extensions" Prelude..=) Prelude.<$> extensions,
            ("Subject" Prelude..=) Prelude.<$> subject
          ]
      )
