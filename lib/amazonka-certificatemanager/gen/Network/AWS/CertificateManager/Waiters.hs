{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Waiters
  ( -- * CertificateValidated
    mkCertificateValidated,
  )
where

import Network.AWS.CertificateManager.DescribeCertificate
import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CertificateManager.DescribeCertificate' every 60 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCertificateValidated :: Waiter.Wait DescribeCertificate
mkCertificateValidated =
  Waiter.Wait
    { Waiter._waitName = "CertificateValidated",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "SUCCESS"
            Waiter.AcceptSuccess
            ( Lens.field @"certificate" Core.. Lens._Just
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"domainValidationOptions" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"validationStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "PENDING_VALIDATION"
            Waiter.AcceptRetry
            ( Lens.field @"certificate" Core.. Lens._Just
                Core.. Lens.folding
                  ( Lens.concatOf
                      ( Lens.field @"domainValidationOptions" Core.. Lens._Just
                          Core.. Lens.to Core.toList
                      )
                  )
                Core.. Lens.field @"validationStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.field @"certificate" Core.. Lens._Just
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptFailure
        ]
    }
