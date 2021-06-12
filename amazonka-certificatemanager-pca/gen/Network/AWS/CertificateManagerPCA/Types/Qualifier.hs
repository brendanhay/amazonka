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
-- Module      : Network.AWS.CertificateManagerPCA.Types.Qualifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.Qualifier where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines a @PolicyInformation@ qualifier. ACM Private CA supports the
-- <https://tools.ietf.org/html/rfc5280#section-4.2.1.4 certification practice statement (CPS) qualifier>
-- defined in RFC 5280.
--
-- /See:/ 'newQualifier' smart constructor.
data Qualifier = Qualifier'
  { -- | Contains a pointer to a certification practice statement (CPS) published
    -- by the CA.
    cpsUri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Qualifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpsUri', 'qualifier_cpsUri' - Contains a pointer to a certification practice statement (CPS) published
-- by the CA.
newQualifier ::
  -- | 'cpsUri'
  Core.Text ->
  Qualifier
newQualifier pCpsUri_ = Qualifier' {cpsUri = pCpsUri_}

-- | Contains a pointer to a certification practice statement (CPS) published
-- by the CA.
qualifier_cpsUri :: Lens.Lens' Qualifier Core.Text
qualifier_cpsUri = Lens.lens (\Qualifier' {cpsUri} -> cpsUri) (\s@Qualifier' {} a -> s {cpsUri = a} :: Qualifier)

instance Core.Hashable Qualifier

instance Core.NFData Qualifier

instance Core.ToJSON Qualifier where
  toJSON Qualifier' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CpsUri" Core..= cpsUri)]
      )
