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
import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.CertificateManager.DescribeCertificate' every 60 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCertificateValidated :: Wait.Wait DescribeCertificate
mkCertificateValidated =
  Wait.Wait
    { Wait._waitName = "CertificateValidated",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 60,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "SUCCESS"
            Wait.AcceptSuccess
            ( dcrsCertificate Lude.. Lens._Just
                Lude.. Lens.folding
                  ( Lens.concatOf
                      ( cdDomainValidationOptions Lude.. Lens._Just
                          Lude.. Lens.to Lude.toList
                      )
                  )
                Lude.. dvValidationStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "PENDING_VALIDATION"
            Wait.AcceptRetry
            ( dcrsCertificate Lude.. Lens._Just
                Lude.. Lens.folding
                  ( Lens.concatOf
                      ( cdDomainValidationOptions Lude.. Lens._Just
                          Lude.. Lens.to Lude.toList
                      )
                  )
                Lude.. dvValidationStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "FAILED"
            Wait.AcceptFailure
            ( dcrsCertificate Lude.. Lens._Just
                Lude.. cdStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ResourceNotFoundException" Wait.AcceptFailure
        ]
    }
