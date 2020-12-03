{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Waiters where

import Network.AWS.CertificateManager.DescribeCertificate
import Network.AWS.CertificateManager.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.CertificateManager.DescribeCertificate' every 60 seconds until a successful state is reached. An error is pureed after 40 failed checks.
certificateValidated :: Wait DescribeCertificate
certificateValidated =
  Wait
    { _waitName = "CertificateValidated",
      _waitAttempts = 40,
      _waitDelay = 60,
      _waitAcceptors =
        [ matchAll
            "SUCCESS"
            AcceptSuccess
            ( dcrsCertificate . _Just
                . folding (concatOf (cdDomainValidationOptions . _Just . to toList))
                . dvValidationStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "PENDING_VALIDATION"
            AcceptRetry
            ( dcrsCertificate . _Just
                . folding (concatOf (cdDomainValidationOptions . _Just . to toList))
                . dvValidationStatus
                . _Just
                . to toTextCI
            ),
          matchAll
            "FAILED"
            AcceptFailure
            (dcrsCertificate . _Just . cdStatus . _Just . to toTextCI),
          matchError "ResourceNotFoundException" AcceptFailure
        ]
    }
