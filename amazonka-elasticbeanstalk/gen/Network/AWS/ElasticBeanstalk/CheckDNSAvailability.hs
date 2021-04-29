{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticBeanstalk.CheckDNSAvailability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks if the specified CNAME is available.
module Network.AWS.ElasticBeanstalk.CheckDNSAvailability
  ( -- * Creating a Request
    CheckDNSAvailability (..),
    newCheckDNSAvailability,

    -- * Request Lenses
    checkDNSAvailability_cNAMEPrefix,

    -- * Destructuring the Response
    CheckDNSAvailabilityResponse (..),
    newCheckDNSAvailabilityResponse,

    -- * Response Lenses
    checkDNSAvailabilityResponse_available,
    checkDNSAvailabilityResponse_fullyQualifiedCNAME,
    checkDNSAvailabilityResponse_httpStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Results message indicating whether a CNAME is available.
--
-- /See:/ 'newCheckDNSAvailability' smart constructor.
data CheckDNSAvailability = CheckDNSAvailability'
  { -- | The prefix used when this CNAME is reserved.
    cNAMEPrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CheckDNSAvailability where
  type
    Rs CheckDNSAvailability =
      CheckDNSAvailabilityResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CheckDNSAvailabilityResult"
      ( \s h x ->
          CheckDNSAvailabilityResponse'
            Prelude.<$> (x Prelude..@? "Available")
            Prelude.<*> (x Prelude..@? "FullyQualifiedCNAME")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CheckDNSAvailability

instance Prelude.NFData CheckDNSAvailability

instance Prelude.ToHeaders CheckDNSAvailability where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CheckDNSAvailability where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CheckDNSAvailability where
  toQuery CheckDNSAvailability' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CheckDNSAvailability" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "CNAMEPrefix" Prelude.=: cNAMEPrefix
      ]

-- | Indicates if the specified CNAME is available.
--
-- /See:/ 'newCheckDNSAvailabilityResponse' smart constructor.
data CheckDNSAvailabilityResponse = CheckDNSAvailabilityResponse'
  { -- | Indicates if the specified CNAME is available:
    --
    -- -   @true@ : The CNAME is available.
    --
    -- -   @false@ : The CNAME is not available.
    available :: Prelude.Maybe Prelude.Bool,
    -- | The fully qualified CNAME to reserve when CreateEnvironment is called
    -- with the provided prefix.
    fullyQualifiedCNAME :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CheckDNSAvailabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'available', 'checkDNSAvailabilityResponse_available' - Indicates if the specified CNAME is available:
--
-- -   @true@ : The CNAME is available.
--
-- -   @false@ : The CNAME is not available.
--
-- 'fullyQualifiedCNAME', 'checkDNSAvailabilityResponse_fullyQualifiedCNAME' - The fully qualified CNAME to reserve when CreateEnvironment is called
-- with the provided prefix.
--
-- 'httpStatus', 'checkDNSAvailabilityResponse_httpStatus' - The response's http status code.
newCheckDNSAvailabilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CheckDNSAvailabilityResponse
newCheckDNSAvailabilityResponse pHttpStatus_ =
  CheckDNSAvailabilityResponse'
    { available =
        Prelude.Nothing,
      fullyQualifiedCNAME = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates if the specified CNAME is available:
--
-- -   @true@ : The CNAME is available.
--
-- -   @false@ : The CNAME is not available.
checkDNSAvailabilityResponse_available :: Lens.Lens' CheckDNSAvailabilityResponse (Prelude.Maybe Prelude.Bool)
checkDNSAvailabilityResponse_available = Lens.lens (\CheckDNSAvailabilityResponse' {available} -> available) (\s@CheckDNSAvailabilityResponse' {} a -> s {available = a} :: CheckDNSAvailabilityResponse)

-- | The fully qualified CNAME to reserve when CreateEnvironment is called
-- with the provided prefix.
checkDNSAvailabilityResponse_fullyQualifiedCNAME :: Lens.Lens' CheckDNSAvailabilityResponse (Prelude.Maybe Prelude.Text)
checkDNSAvailabilityResponse_fullyQualifiedCNAME = Lens.lens (\CheckDNSAvailabilityResponse' {fullyQualifiedCNAME} -> fullyQualifiedCNAME) (\s@CheckDNSAvailabilityResponse' {} a -> s {fullyQualifiedCNAME = a} :: CheckDNSAvailabilityResponse)

-- | The response's http status code.
checkDNSAvailabilityResponse_httpStatus :: Lens.Lens' CheckDNSAvailabilityResponse Prelude.Int
checkDNSAvailabilityResponse_httpStatus = Lens.lens (\CheckDNSAvailabilityResponse' {httpStatus} -> httpStatus) (\s@CheckDNSAvailabilityResponse' {} a -> s {httpStatus = a} :: CheckDNSAvailabilityResponse)

instance Prelude.NFData CheckDNSAvailabilityResponse
