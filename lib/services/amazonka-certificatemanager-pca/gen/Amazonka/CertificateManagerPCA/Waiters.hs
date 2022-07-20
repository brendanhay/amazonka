{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManagerPCA.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Waiters where

import Amazonka.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
import Amazonka.CertificateManagerPCA.GetCertificate
import Amazonka.CertificateManagerPCA.GetCertificateAuthorityCsr
import Amazonka.CertificateManagerPCA.Lens
import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
newAuditReportCreated :: Core.Wait DescribeCertificateAuthorityAuditReport
newAuditReportCreated =
  Core.Wait
    { Core._waitName = "AuditReportCreated",
      Core._waitAttempts = 60,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchAll
            "SUCCESS"
            Core.AcceptSuccess
            ( describeCertificateAuthorityAuditReportResponse_auditReportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeCertificateAuthorityAuditReportResponse_auditReportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.CertificateManagerPCA.GetCertificate' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
newCertificateIssued :: Core.Wait GetCertificate
newCertificateIssued =
  Core.Wait
    { Core._waitName = "CertificateIssued",
      Core._waitAttempts = 60,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "RequestInProgressException"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.CertificateManagerPCA.GetCertificateAuthorityCsr' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
newCertificateAuthorityCSRCreated :: Core.Wait GetCertificateAuthorityCsr
newCertificateAuthorityCSRCreated =
  Core.Wait
    { Core._waitName =
        "CertificateAuthorityCSRCreated",
      Core._waitAttempts = 60,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "RequestInProgressException"
            Core.AcceptRetry
        ]
    }
