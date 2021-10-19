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
-- Module      : Network.AWS.APIGateway.GetDomainName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a domain name that is contained in a simpler, more intuitive
-- URL that can be called.
module Network.AWS.APIGateway.GetDomainName
  ( -- * Creating a Request
    GetDomainName (..),
    newGetDomainName,

    -- * Request Lenses
    getDomainName_domainName,

    -- * Destructuring the Response
    DomainName (..),
    newDomainName,

    -- * Response Lenses
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_certificateArn,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,
    domainName_domainName,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_certificateUploadDate,
    domainName_distributionDomainName,
    domainName_domainNameStatusMessage,
    domainName_endpointConfiguration,
    domainName_domainNameStatus,
    domainName_tags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to get the name of a DomainName resource.
--
-- /See:/ 'newGetDomainName' smart constructor.
data GetDomainName = GetDomainName'
  { -- | [Required] The name of the DomainName resource.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getDomainName_domainName' - [Required] The name of the DomainName resource.
newGetDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  GetDomainName
newGetDomainName pDomainName_ =
  GetDomainName' {domainName = pDomainName_}

-- | [Required] The name of the DomainName resource.
getDomainName_domainName :: Lens.Lens' GetDomainName Prelude.Text
getDomainName_domainName = Lens.lens (\GetDomainName' {domainName} -> domainName) (\s@GetDomainName' {} a -> s {domainName = a} :: GetDomainName)

instance Core.AWSRequest GetDomainName where
  type AWSResponse GetDomainName = DomainName
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetDomainName

instance Prelude.NFData GetDomainName

instance Core.ToHeaders GetDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetDomainName where
  toPath GetDomainName' {..} =
    Prelude.mconcat
      ["/domainnames/", Core.toBS domainName]

instance Core.ToQuery GetDomainName where
  toQuery = Prelude.const Prelude.mempty
