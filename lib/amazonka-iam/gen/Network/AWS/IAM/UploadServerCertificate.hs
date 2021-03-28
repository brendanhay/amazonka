{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UploadServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a server certificate entity for the AWS account. The server certificate entity includes a public key certificate, a private key, and an optional certificate chain, which should all be PEM-encoded.
--
-- We recommend that you use <https://docs.aws.amazon.com/acm/ AWS Certificate Manager> to provision, manage, and deploy your server certificates. With ACM you can request a certificate, deploy it to AWS resources, and let ACM handle certificate renewals for you. Certificates provided by ACM are free. For more information about using ACM, see the <https://docs.aws.amazon.com/acm/latest/userguide/ AWS Certificate Manager User Guide> .
-- For more information about working with server certificates, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with Server Certificates> in the /IAM User Guide/ . This topic includes a list of AWS services that can use the server certificates that you manage with IAM.
-- For information about the number of server certificates you can upload, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-limits.html Limitations on IAM Entities and Objects> in the /IAM User Guide/ .
module Network.AWS.IAM.UploadServerCertificate
    (
    -- * Creating a request
      UploadServerCertificate (..)
    , mkUploadServerCertificate
    -- ** Request lenses
    , uscServerCertificateName
    , uscCertificateBody
    , uscPrivateKey
    , uscCertificateChain
    , uscPath

    -- * Destructuring the response
    , UploadServerCertificateResponse (..)
    , mkUploadServerCertificateResponse
    -- ** Response lenses
    , ursServerCertificateMetadata
    , ursResponseStatus
    ) where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUploadServerCertificate' smart constructor.
data UploadServerCertificate = UploadServerCertificate'
  { serverCertificateName :: Types.ServerCertificateNameType
    -- ^ The name for the server certificate. Do not include the path in this value. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
  , certificateBody :: Types.CertificateBodyType
    -- ^ The contents of the public key certificate in PEM-encoded format.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
  , privateKey :: Types.PrivateKeyType
    -- ^ The contents of the private key in PEM-encoded format.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
  , certificateChain :: Core.Maybe Types.CertificateChainType
    -- ^ The contents of the certificate chain. This is typically a concatenation of the PEM-encoded public key certificates of the chain.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
  , path :: Core.Maybe Types.Path
    -- ^ The path for the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadServerCertificate' value with any optional fields omitted.
mkUploadServerCertificate
    :: Types.ServerCertificateNameType -- ^ 'serverCertificateName'
    -> Types.CertificateBodyType -- ^ 'certificateBody'
    -> Types.PrivateKeyType -- ^ 'privateKey'
    -> UploadServerCertificate
mkUploadServerCertificate serverCertificateName certificateBody
  privateKey
  = UploadServerCertificate'{serverCertificateName, certificateBody,
                             privateKey, certificateChain = Core.Nothing, path = Core.Nothing}

-- | The name for the server certificate. Do not include the path in this value. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscServerCertificateName :: Lens.Lens' UploadServerCertificate Types.ServerCertificateNameType
uscServerCertificateName = Lens.field @"serverCertificateName"
{-# INLINEABLE uscServerCertificateName #-}
{-# DEPRECATED serverCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead"  #-}

-- | The contents of the public key certificate in PEM-encoded format.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'certificateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscCertificateBody :: Lens.Lens' UploadServerCertificate Types.CertificateBodyType
uscCertificateBody = Lens.field @"certificateBody"
{-# INLINEABLE uscCertificateBody #-}
{-# DEPRECATED certificateBody "Use generic-lens or generic-optics with 'certificateBody' instead"  #-}

-- | The contents of the private key in PEM-encoded format.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscPrivateKey :: Lens.Lens' UploadServerCertificate Types.PrivateKeyType
uscPrivateKey = Lens.field @"privateKey"
{-# INLINEABLE uscPrivateKey #-}
{-# DEPRECATED privateKey "Use generic-lens or generic-optics with 'privateKey' instead"  #-}

-- | The contents of the certificate chain. This is typically a concatenation of the PEM-encoded public key certificates of the chain.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of the following:
--
--     * Any printable ASCII character ranging from the space character (@\u0020@ ) through the end of the ASCII character range
--
--
--     * The printable characters in the Basic Latin and Latin-1 Supplement character set (through @\u00FF@ )
--
--
--     * The special characters tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ )
--
--
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscCertificateChain :: Lens.Lens' UploadServerCertificate (Core.Maybe Types.CertificateChainType)
uscCertificateChain = Lens.field @"certificateChain"
{-# INLINEABLE uscCertificateChain #-}
{-# DEPRECATED certificateChain "Use generic-lens or generic-optics with 'certificateChain' instead"  #-}

-- | The path for the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uscPath :: Lens.Lens' UploadServerCertificate (Core.Maybe Types.Path)
uscPath = Lens.field @"path"
{-# INLINEABLE uscPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.ToQuery UploadServerCertificate where
        toQuery UploadServerCertificate{..}
          = Core.toQueryPair "Action"
              ("UploadServerCertificate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-08" :: Core.Text)
              Core.<>
              Core.toQueryPair "ServerCertificateName" serverCertificateName
              Core.<> Core.toQueryPair "CertificateBody" certificateBody
              Core.<> Core.toQueryPair "PrivateKey" privateKey
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CertificateChain")
                certificateChain
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Path") path

instance Core.ToHeaders UploadServerCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest UploadServerCertificate where
        type Rs UploadServerCertificate = UploadServerCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "UploadServerCertificateResult"
              (\ s h x ->
                 UploadServerCertificateResponse' Core.<$>
                   (x Core..@? "ServerCertificateMetadata") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a successful 'UploadServerCertificate' request. 
--
-- /See:/ 'mkUploadServerCertificateResponse' smart constructor.
data UploadServerCertificateResponse = UploadServerCertificateResponse'
  { serverCertificateMetadata :: Core.Maybe Types.ServerCertificateMetadata
    -- ^ The meta information of the uploaded server certificate without its certificate body, certificate chain, and private key.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UploadServerCertificateResponse' value with any optional fields omitted.
mkUploadServerCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UploadServerCertificateResponse
mkUploadServerCertificateResponse responseStatus
  = UploadServerCertificateResponse'{serverCertificateMetadata =
                                       Core.Nothing,
                                     responseStatus}

-- | The meta information of the uploaded server certificate without its certificate body, certificate chain, and private key.
--
-- /Note:/ Consider using 'serverCertificateMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursServerCertificateMetadata :: Lens.Lens' UploadServerCertificateResponse (Core.Maybe Types.ServerCertificateMetadata)
ursServerCertificateMetadata = Lens.field @"serverCertificateMetadata"
{-# INLINEABLE ursServerCertificateMetadata #-}
{-# DEPRECATED serverCertificateMetadata "Use generic-lens or generic-optics with 'serverCertificateMetadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UploadServerCertificateResponse Core.Int
ursResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ursResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
