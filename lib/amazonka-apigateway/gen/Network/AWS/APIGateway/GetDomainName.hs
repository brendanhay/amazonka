{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetDomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a domain name that is contained in a simpler, more intuitive URL that can be called.
module Network.AWS.APIGateway.GetDomainName
  ( -- * Creating a request
    GetDomainName (..),
    mkGetDomainName,

    -- ** Request lenses
    gdnDomainName,

    -- * Destructuring the response
    DomainName (..),
    mkDomainName,

    -- ** Response lenses
    dnRegionalHostedZoneId,
    dnCertificateName,
    dnRegionalCertificateARN,
    dnCertificateARN,
    dnDistributionHostedZoneId,
    dnSecurityPolicy,
    dnDomainName,
    dnMutualTLSAuthentication,
    dnRegionalCertificateName,
    dnRegionalDomainName,
    dnCertificateUploadDate,
    dnDistributionDomainName,
    dnDomainNameStatusMessage,
    dnEndpointConfiguration,
    dnDomainNameStatus,
    dnTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to get the name of a 'DomainName' resource.
--
-- /See:/ 'mkGetDomainName' smart constructor.
newtype GetDomainName = GetDomainName' {domainName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomainName' with the minimum fields required to make a request.
--
-- * 'domainName' - [Required] The name of the 'DomainName' resource.
mkGetDomainName ::
  -- | 'domainName'
  Lude.Text ->
  GetDomainName
mkGetDomainName pDomainName_ =
  GetDomainName' {domainName = pDomainName_}

-- | [Required] The name of the 'DomainName' resource.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnDomainName :: Lens.Lens' GetDomainName Lude.Text
gdnDomainName = Lens.lens (domainName :: GetDomainName -> Lude.Text) (\s a -> s {domainName = a} :: GetDomainName)
{-# DEPRECATED gdnDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest GetDomainName where
  type Rs GetDomainName = DomainName
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetDomainName where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetDomainName where
  toPath GetDomainName' {..} =
    Lude.mconcat ["/domainnames/", Lude.toBS domainName]

instance Lude.ToQuery GetDomainName where
  toQuery = Lude.const Lude.mempty
