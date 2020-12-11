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
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR
import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificate' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkCertificateIssued :: Wait.Wait GetCertificate
mkCertificateIssued =
  Wait.Wait
    { Wait._waitName = "CertificateIssued",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 3,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "RequestInProgressException" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkAuditReportCreated :: Wait.Wait DescribeCertificateAuthorityAuditReport
mkAuditReportCreated =
  Wait.Wait
    { Wait._waitName = "AuditReportCreated",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 3,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "SUCCESS"
            Wait.AcceptSuccess
            ( dcaarrsAuditReportStatus Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "FAILED"
            Wait.AcceptFailure
            ( dcaarrsAuditReportStatus Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkCertificateAuthorityCSRCreated :: Wait.Wait GetCertificateAuthorityCSR
mkCertificateAuthorityCSRCreated =
  Wait.Wait
    { Wait._waitName = "CertificateAuthorityCSRCreated",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 3,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "RequestInProgressException" Wait.AcceptRetry
        ]
    }
