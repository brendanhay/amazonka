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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
newAuditReportCreated :: Waiter.Wait DescribeCertificateAuthorityAuditReport
newAuditReportCreated =
  Waiter.Wait
    { Waiter._waitName =
        "AuditReportCreated",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "SUCCESS"
            Waiter.AcceptSuccess
            ( describeCertificateAuthorityAuditReportResponse_auditReportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( describeCertificateAuthorityAuditReportResponse_auditReportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
newCertificateAuthorityCSRCreated :: Waiter.Wait GetCertificateAuthorityCsr
newCertificateAuthorityCSRCreated =
  Waiter.Wait
    { Waiter._waitName =
        "CertificateAuthorityCSRCreated",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "RequestInProgressException"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificate' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
newCertificateIssued :: Waiter.Wait GetCertificate
newCertificateIssued =
  Waiter.Wait
    { Waiter._waitName = "CertificateIssued",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "RequestInProgressException"
            Waiter.AcceptRetry
        ]
    }
