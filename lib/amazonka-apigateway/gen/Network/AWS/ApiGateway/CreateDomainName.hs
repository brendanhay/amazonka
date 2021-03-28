{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateDomainName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new domain name.
module Network.AWS.ApiGateway.CreateDomainName
    (
    -- * Creating a request
      CreateDomainName (..)
    , mkCreateDomainName
    -- ** Request lenses
    , cdnDomainName
    , cdnCertificateArn
    , cdnCertificateBody
    , cdnCertificateChain
    , cdnCertificateName
    , cdnCertificatePrivateKey
    , cdnEndpointConfiguration
    , cdnMutualTlsAuthentication
    , cdnRegionalCertificateArn
    , cdnRegionalCertificateName
    , cdnSecurityPolicy
    , cdnTags

     -- * Destructuring the response
    , Types.DomainName (..)
    , Types.mkDomainName
    -- ** Response lenses
    , Types.dnCertificateArn
    , Types.dnCertificateName
    , Types.dnCertificateUploadDate
    , Types.dnDistributionDomainName
    , Types.dnDistributionHostedZoneId
    , Types.dnDomainName
    , Types.dnDomainNameStatus
    , Types.dnDomainNameStatusMessage
    , Types.dnEndpointConfiguration
    , Types.dnMutualTlsAuthentication
    , Types.dnRegionalCertificateArn
    , Types.dnRegionalCertificateName
    , Types.dnRegionalDomainName
    , Types.dnRegionalHostedZoneId
    , Types.dnSecurityPolicy
    , Types.dnTags
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a new domain name.
--
-- /See:/ 'mkCreateDomainName' smart constructor.
data CreateDomainName = CreateDomainName'
  { domainName :: Core.Text
    -- ^ [Required] The name of the 'DomainName' resource.
  , certificateArn :: Core.Maybe Core.Text
    -- ^ The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
  , certificateBody :: Core.Maybe Core.Text
    -- ^ [Deprecated] The body of the server certificate that will be used by edge-optimized endpoint for this domain name provided by your certificate authority.
  , certificateChain :: Core.Maybe Core.Text
    -- ^ [Deprecated] The intermediate certificates and optionally the root certificate, one after the other without any blank lines, used by an edge-optimized endpoint for this domain name. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
  , certificateName :: Core.Maybe Core.Text
    -- ^ The user-friendly name of the certificate that will be used by edge-optimized endpoint for this domain name.
  , certificatePrivateKey :: Core.Maybe Core.Text
    -- ^ [Deprecated] Your edge-optimized endpoint's domain name certificate's private key.
  , endpointConfiguration :: Core.Maybe Types.EndpointConfiguration
    -- ^ The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
  , mutualTlsAuthentication :: Core.Maybe Types.MutualTlsAuthenticationInput
  , regionalCertificateArn :: Core.Maybe Core.Text
    -- ^ The reference to an AWS-managed certificate that will be used by regional endpoint for this domain name. AWS Certificate Manager is the only supported source.
  , regionalCertificateName :: Core.Maybe Core.Text
    -- ^ The user-friendly name of the certificate that will be used by regional endpoint for this domain name.
  , securityPolicy :: Core.Maybe Types.SecurityPolicy
    -- ^ The Transport Layer Security (TLS) version + cipher suite for this 'DomainName' . The valid values are @TLS_1_0@ and @TLS_1_2@ .
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDomainName' value with any optional fields omitted.
mkCreateDomainName
    :: Core.Text -- ^ 'domainName'
    -> CreateDomainName
mkCreateDomainName domainName
  = CreateDomainName'{domainName, certificateArn = Core.Nothing,
                      certificateBody = Core.Nothing, certificateChain = Core.Nothing,
                      certificateName = Core.Nothing,
                      certificatePrivateKey = Core.Nothing,
                      endpointConfiguration = Core.Nothing,
                      mutualTlsAuthentication = Core.Nothing,
                      regionalCertificateArn = Core.Nothing,
                      regionalCertificateName = Core.Nothing,
                      securityPolicy = Core.Nothing, tags = Core.Nothing}

-- | [Required] The name of the 'DomainName' resource.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnDomainName :: Lens.Lens' CreateDomainName Core.Text
cdnDomainName = Lens.field @"domainName"
{-# INLINEABLE cdnDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The reference to an AWS-managed certificate that will be used by edge-optimized endpoint for this domain name. AWS Certificate Manager is the only supported source.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificateArn :: Lens.Lens' CreateDomainName (Core.Maybe Core.Text)
cdnCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE cdnCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | [Deprecated] The body of the server certificate that will be used by edge-optimized endpoint for this domain name provided by your certificate authority.
--
-- /Note:/ Consider using 'certificateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificateBody :: Lens.Lens' CreateDomainName (Core.Maybe Core.Text)
cdnCertificateBody = Lens.field @"certificateBody"
{-# INLINEABLE cdnCertificateBody #-}
{-# DEPRECATED certificateBody "Use generic-lens or generic-optics with 'certificateBody' instead"  #-}

-- | [Deprecated] The intermediate certificates and optionally the root certificate, one after the other without any blank lines, used by an edge-optimized endpoint for this domain name. If you include the root certificate, your certificate chain must start with intermediate certificates and end with the root certificate. Use the intermediate certificates that were provided by your certificate authority. Do not include any intermediaries that are not in the chain of trust path.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificateChain :: Lens.Lens' CreateDomainName (Core.Maybe Core.Text)
cdnCertificateChain = Lens.field @"certificateChain"
{-# INLINEABLE cdnCertificateChain #-}
{-# DEPRECATED certificateChain "Use generic-lens or generic-optics with 'certificateChain' instead"  #-}

-- | The user-friendly name of the certificate that will be used by edge-optimized endpoint for this domain name.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificateName :: Lens.Lens' CreateDomainName (Core.Maybe Core.Text)
cdnCertificateName = Lens.field @"certificateName"
{-# INLINEABLE cdnCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

-- | [Deprecated] Your edge-optimized endpoint's domain name certificate's private key.
--
-- /Note:/ Consider using 'certificatePrivateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnCertificatePrivateKey :: Lens.Lens' CreateDomainName (Core.Maybe Core.Text)
cdnCertificatePrivateKey = Lens.field @"certificatePrivateKey"
{-# INLINEABLE cdnCertificatePrivateKey #-}
{-# DEPRECATED certificatePrivateKey "Use generic-lens or generic-optics with 'certificatePrivateKey' instead"  #-}

-- | The endpoint configuration of this 'DomainName' showing the endpoint types of the domain name.
--
-- /Note:/ Consider using 'endpointConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnEndpointConfiguration :: Lens.Lens' CreateDomainName (Core.Maybe Types.EndpointConfiguration)
cdnEndpointConfiguration = Lens.field @"endpointConfiguration"
{-# INLINEABLE cdnEndpointConfiguration #-}
{-# DEPRECATED endpointConfiguration "Use generic-lens or generic-optics with 'endpointConfiguration' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'mutualTlsAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnMutualTlsAuthentication :: Lens.Lens' CreateDomainName (Core.Maybe Types.MutualTlsAuthenticationInput)
cdnMutualTlsAuthentication = Lens.field @"mutualTlsAuthentication"
{-# INLINEABLE cdnMutualTlsAuthentication #-}
{-# DEPRECATED mutualTlsAuthentication "Use generic-lens or generic-optics with 'mutualTlsAuthentication' instead"  #-}

-- | The reference to an AWS-managed certificate that will be used by regional endpoint for this domain name. AWS Certificate Manager is the only supported source.
--
-- /Note:/ Consider using 'regionalCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnRegionalCertificateArn :: Lens.Lens' CreateDomainName (Core.Maybe Core.Text)
cdnRegionalCertificateArn = Lens.field @"regionalCertificateArn"
{-# INLINEABLE cdnRegionalCertificateArn #-}
{-# DEPRECATED regionalCertificateArn "Use generic-lens or generic-optics with 'regionalCertificateArn' instead"  #-}

-- | The user-friendly name of the certificate that will be used by regional endpoint for this domain name.
--
-- /Note:/ Consider using 'regionalCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnRegionalCertificateName :: Lens.Lens' CreateDomainName (Core.Maybe Core.Text)
cdnRegionalCertificateName = Lens.field @"regionalCertificateName"
{-# INLINEABLE cdnRegionalCertificateName #-}
{-# DEPRECATED regionalCertificateName "Use generic-lens or generic-optics with 'regionalCertificateName' instead"  #-}

-- | The Transport Layer Security (TLS) version + cipher suite for this 'DomainName' . The valid values are @TLS_1_0@ and @TLS_1_2@ .
--
-- /Note:/ Consider using 'securityPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnSecurityPolicy :: Lens.Lens' CreateDomainName (Core.Maybe Types.SecurityPolicy)
cdnSecurityPolicy = Lens.field @"securityPolicy"
{-# INLINEABLE cdnSecurityPolicy #-}
{-# DEPRECATED securityPolicy "Use generic-lens or generic-optics with 'securityPolicy' instead"  #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdnTags :: Lens.Lens' CreateDomainName (Core.Maybe (Core.HashMap Core.Text Core.Text))
cdnTags = Lens.field @"tags"
{-# INLINEABLE cdnTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDomainName where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDomainName where
        toHeaders CreateDomainName{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateDomainName where
        toJSON CreateDomainName{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domainName" Core..= domainName),
                  ("certificateArn" Core..=) Core.<$> certificateArn,
                  ("certificateBody" Core..=) Core.<$> certificateBody,
                  ("certificateChain" Core..=) Core.<$> certificateChain,
                  ("certificateName" Core..=) Core.<$> certificateName,
                  ("certificatePrivateKey" Core..=) Core.<$> certificatePrivateKey,
                  ("endpointConfiguration" Core..=) Core.<$> endpointConfiguration,
                  ("mutualTlsAuthentication" Core..=) Core.<$>
                    mutualTlsAuthentication,
                  ("regionalCertificateArn" Core..=) Core.<$> regionalCertificateArn,
                  ("regionalCertificateName" Core..=) Core.<$>
                    regionalCertificateName,
                  ("securityPolicy" Core..=) Core.<$> securityPolicy,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateDomainName where
        type Rs CreateDomainName = Types.DomainName
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/domainnames",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
