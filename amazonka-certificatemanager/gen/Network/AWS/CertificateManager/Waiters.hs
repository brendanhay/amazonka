{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Waiters where

import Network.AWS.CertificateManager.DescribeCertificate
import Network.AWS.CertificateManager.Lens
import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CertificateManager.DescribeCertificate' every 60 seconds until a successful state is reached. An error is returned after 40 failed checks.
newCertificateValidated :: Waiter.Wait DescribeCertificate
newCertificateValidated =
  Waiter.Wait
    { Waiter._waitName =
        "CertificateValidated",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "SUCCESS"
            Waiter.AcceptSuccess
            ( describeCertificateResponse_certificate
                Prelude.. Lens._Just
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      ( certificateDetail_domainValidationOptions
                          Prelude.. Lens._Just
                          Prelude.. Lens.to Prelude.toList
                      )
                  )
                Prelude.. domainValidation_validationStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "PENDING_VALIDATION"
            Waiter.AcceptRetry
            ( describeCertificateResponse_certificate
                Prelude.. Lens._Just
                Prelude.. Lens.folding
                  ( Lens.concatOf
                      ( certificateDetail_domainValidationOptions
                          Prelude.. Lens._Just
                          Prelude.. Lens.to Prelude.toList
                      )
                  )
                Prelude.. domainValidation_validationStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( describeCertificateResponse_certificate
                Prelude.. Lens._Just
                Prelude.. certificateDetail_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptFailure
        ]
    }
