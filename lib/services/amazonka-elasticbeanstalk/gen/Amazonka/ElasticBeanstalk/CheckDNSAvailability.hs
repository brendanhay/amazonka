{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticBeanstalk.CheckDNSAvailability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks if the specified CNAME is available.
module Amazonka.ElasticBeanstalk.CheckDNSAvailability
  ( -- * Creating a Request
    CheckDNSAvailability (..),
    newCheckDNSAvailability,

    -- * Request Lenses
    checkDNSAvailability_cNAMEPrefix,

    -- * Destructuring the Response
    CheckDNSAvailabilityResponse (..),
    newCheckDNSAvailabilityResponse,

    -- * Response Lenses
    checkDNSAvailabilityResponse_fullyQualifiedCNAME,
    checkDNSAvailabilityResponse_available,
    checkDNSAvailabilityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Results message indicating whether a CNAME is available.
--
-- /See:/ 'newCheckDNSAvailability' smart constructor.
data CheckDNSAvailability = CheckDNSAvailability'
  { -- | The prefix used when this CNAME is reserved.
    cNAMEPrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckDNSAvailability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cNAMEPrefix', 'checkDNSAvailability_cNAMEPrefix' - The prefix used when this CNAME is reserved.
newCheckDNSAvailability ::
  -- | 'cNAMEPrefix'
  Prelude.Text ->
  CheckDNSAvailability
newCheckDNSAvailability pCNAMEPrefix_ =
  CheckDNSAvailability' {cNAMEPrefix = pCNAMEPrefix_}

-- | The prefix used when this CNAME is reserved.
checkDNSAvailability_cNAMEPrefix :: Lens.Lens' CheckDNSAvailability Prelude.Text
checkDNSAvailability_cNAMEPrefix = Lens.lens (\CheckDNSAvailability' {cNAMEPrefix} -> cNAMEPrefix) (\s@CheckDNSAvailability' {} a -> s {cNAMEPrefix = a} :: CheckDNSAvailability)

instance Core.AWSRequest CheckDNSAvailability where
  type
    AWSResponse CheckDNSAvailability =
      CheckDNSAvailabilityResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CheckDNSAvailabilityResult"
      ( \s h x ->
          CheckDNSAvailabilityResponse'
            Prelude.<$> (x Core..@? "FullyQualifiedCNAME")
            Prelude.<*> (x Core..@? "Available")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CheckDNSAvailability where
  hashWithSalt salt' CheckDNSAvailability' {..} =
    salt' `Prelude.hashWithSalt` cNAMEPrefix

instance Prelude.NFData CheckDNSAvailability where
  rnf CheckDNSAvailability' {..} =
    Prelude.rnf cNAMEPrefix

instance Core.ToHeaders CheckDNSAvailability where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CheckDNSAvailability where
  toPath = Prelude.const "/"

instance Core.ToQuery CheckDNSAvailability where
  toQuery CheckDNSAvailability' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CheckDNSAvailability" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "CNAMEPrefix" Core.=: cNAMEPrefix
      ]

-- | Indicates if the specified CNAME is available.
--
-- /See:/ 'newCheckDNSAvailabilityResponse' smart constructor.
data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse'
  { -- | The fully qualified CNAME to reserve when CreateEnvironment is called
    -- with the provided prefix.
    fullyQualifiedCNAME :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the specified CNAME is available:
    --
    -- -   @true@ : The CNAME is available.
    --
    -- -   @false@ : The CNAME is not available.
    available :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CheckDNSAvailabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fullyQualifiedCNAME', 'checkDNSAvailabilityResponse_fullyQualifiedCNAME' - The fully qualified CNAME to reserve when CreateEnvironment is called
-- with the provided prefix.
--
-- 'available', 'checkDNSAvailabilityResponse_available' - Indicates if the specified CNAME is available:
--
-- -   @true@ : The CNAME is available.
--
-- -   @false@ : The CNAME is not available.
--
-- 'httpStatus', 'checkDNSAvailabilityResponse_httpStatus' - The response's http status code.
newCheckDNSAvailabilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CheckDNSAvailabilityResponse
newCheckDNSAvailabilityResponse pHttpStatus_ =
  CheckDNSAvailabilityResponse'
    { fullyQualifiedCNAME =
        Prelude.Nothing,
      available = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The fully qualified CNAME to reserve when CreateEnvironment is called
-- with the provided prefix.
checkDNSAvailabilityResponse_fullyQualifiedCNAME :: Lens.Lens' CheckDNSAvailabilityResponse (Prelude.Maybe Prelude.Text)
checkDNSAvailabilityResponse_fullyQualifiedCNAME = Lens.lens (\CheckDNSAvailabilityResponse' {fullyQualifiedCNAME} -> fullyQualifiedCNAME) (\s@CheckDNSAvailabilityResponse' {} a -> s {fullyQualifiedCNAME = a} :: CheckDNSAvailabilityResponse)

-- | Indicates if the specified CNAME is available:
--
-- -   @true@ : The CNAME is available.
--
-- -   @false@ : The CNAME is not available.
checkDNSAvailabilityResponse_available :: Lens.Lens' CheckDNSAvailabilityResponse (Prelude.Maybe Prelude.Bool)
checkDNSAvailabilityResponse_available = Lens.lens (\CheckDNSAvailabilityResponse' {available} -> available) (\s@CheckDNSAvailabilityResponse' {} a -> s {available = a} :: CheckDNSAvailabilityResponse)

-- | The response's http status code.
checkDNSAvailabilityResponse_httpStatus :: Lens.Lens' CheckDNSAvailabilityResponse Prelude.Int
checkDNSAvailabilityResponse_httpStatus = Lens.lens (\CheckDNSAvailabilityResponse' {httpStatus} -> httpStatus) (\s@CheckDNSAvailabilityResponse' {} a -> s {httpStatus = a} :: CheckDNSAvailabilityResponse)

instance Prelude.NFData CheckDNSAvailabilityResponse where
  rnf CheckDNSAvailabilityResponse' {..} =
    Prelude.rnf fullyQualifiedCNAME
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf available
