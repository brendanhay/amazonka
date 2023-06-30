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
-- Module      : Amazonka.CertificateManagerPCA.Types.ApiPassthrough
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.ApiPassthrough where

import Amazonka.CertificateManagerPCA.Types.ASN1Subject
import Amazonka.CertificateManagerPCA.Types.Extensions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains X.509 certificate information to be placed in an issued
-- certificate. An @APIPassthrough@ or @APICSRPassthrough@ template variant
-- must be selected, or else this parameter is ignored.
--
-- If conflicting or duplicate certificate information is supplied from
-- other sources, Amazon Web Services Private CA applies
-- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html#template-order-of-operations order of operation rules>
-- to determine what information is used.
--
-- /See:/ 'newApiPassthrough' smart constructor.
data ApiPassthrough = ApiPassthrough'
  { -- | Specifies X.509 extension information for a certificate.
    extensions :: Prelude.Maybe Extensions,
    subject :: Prelude.Maybe ASN1Subject
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

instance Prelude.Hashable ApiPassthrough where
  hashWithSalt _salt ApiPassthrough' {..} =
    _salt
      `Prelude.hashWithSalt` extensions
      `Prelude.hashWithSalt` subject

instance Prelude.NFData ApiPassthrough where
  rnf ApiPassthrough' {..} =
    Prelude.rnf extensions
      `Prelude.seq` Prelude.rnf subject

instance Data.ToJSON ApiPassthrough where
  toJSON ApiPassthrough' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Extensions" Data..=) Prelude.<$> extensions,
            ("Subject" Data..=) Prelude.<$> subject
          ]
      )
