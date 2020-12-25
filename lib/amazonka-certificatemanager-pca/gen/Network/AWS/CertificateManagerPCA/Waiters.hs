{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Waiters
  ( -- * CertificateIssued
    mkCertificateIssued,

    -- * AuditReportCreated
    mkAuditReportCreated,

    -- * CertificateAuthorityCSRCreated
    mkCertificateAuthorityCSRCreated,
  )
where

import Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
import Network.AWS.CertificateManagerPCA.GetCertificate
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr
import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificate' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkCertificateIssued :: Waiter.Wait GetCertificate
mkCertificateIssued =
  Waiter.Wait
    { Waiter._waitName = "CertificateIssued",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError "RequestInProgressException" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkAuditReportCreated :: Waiter.Wait DescribeCertificateAuthorityAuditReport
mkAuditReportCreated =
  Waiter.Wait
    { Waiter._waitName = "AuditReportCreated",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "SUCCESS"
            Waiter.AcceptSuccess
            (Lens.field @"auditReportStatus" Core.. Lens._Just),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            (Lens.field @"auditReportStatus" Core.. Lens._Just)
        ]
    }

-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkCertificateAuthorityCSRCreated :: Waiter.Wait GetCertificateAuthorityCsr
mkCertificateAuthorityCSRCreated =
  Waiter.Wait
    { Waiter._waitName = "CertificateAuthorityCSRCreated",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError "RequestInProgressException" Waiter.AcceptRetry
        ]
    }
