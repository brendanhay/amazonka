{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Waiters where

import Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
import Network.AWS.CertificateManagerPCA.GetCertificate
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr
import Network.AWS.CertificateManagerPCA.Lens
import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificate' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
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

-- | Polls 'Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
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

-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
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
