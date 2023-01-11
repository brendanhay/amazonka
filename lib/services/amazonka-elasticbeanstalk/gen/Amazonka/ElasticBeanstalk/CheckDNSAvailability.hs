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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    checkDNSAvailabilityResponse_available,
    checkDNSAvailabilityResponse_fullyQualifiedCNAME,
    checkDNSAvailabilityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CheckDNSAvailabilityResult"
      ( \s h x ->
          CheckDNSAvailabilityResponse'
            Prelude.<$> (x Data..@? "Available")
            Prelude.<*> (x Data..@? "FullyQualifiedCNAME")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CheckDNSAvailability where
  hashWithSalt _salt CheckDNSAvailability' {..} =
    _salt `Prelude.hashWithSalt` cNAMEPrefix

instance Prelude.NFData CheckDNSAvailability where
  rnf CheckDNSAvailability' {..} =
    Prelude.rnf cNAMEPrefix

instance Data.ToHeaders CheckDNSAvailability where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CheckDNSAvailability where
  toPath = Prelude.const "/"

instance Data.ToQuery CheckDNSAvailability where
  toQuery CheckDNSAvailability' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CheckDNSAvailability" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "CNAMEPrefix" Data.=: cNAMEPrefix
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CheckDNSAvailabilityResponse where
  rnf CheckDNSAvailabilityResponse' {..} =
    Prelude.rnf available
      `Prelude.seq` Prelude.rnf fullyQualifiedCNAME
      `Prelude.seq` Prelude.rnf httpStatus
