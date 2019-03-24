{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Waiters where

import Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
import Network.AWS.CertificateManagerPCA.GetCertificate
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR
import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificate' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
certificateIssued :: Wait GetCertificate
certificateIssued =
  Wait
    { _waitName = "CertificateIssued"
    , _waitAttempts = 60
    , _waitDelay = 3
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchError "RequestInProgressException" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
auditReportCreated :: Wait DescribeCertificateAuthorityAuditReport
auditReportCreated =
  Wait
    { _waitName = "AuditReportCreated"
    , _waitAttempts = 60
    , _waitDelay = 3
    , _waitAcceptors =
        [ matchAll
            "SUCCESS"
            AcceptSuccess
            (dcaarrsAuditReportStatus . to toTextCI)
        , matchAll
            "FAILED"
            AcceptFailure
            (dcaarrsAuditReportStatus . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR' every 3 seconds until a successful state is reached. An error is returned after 60 failed checks.
certificateAuthorityCSRCreated :: Wait GetCertificateAuthorityCSR
certificateAuthorityCSRCreated =
  Wait
    { _waitName = "CertificateAuthorityCSRCreated"
    , _waitAttempts = 60
    , _waitDelay = 3
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchError "RequestInProgressException" AcceptRetry
        ]
    }

