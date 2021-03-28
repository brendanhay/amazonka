{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServerCertificateMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.ServerCertificateMetadata
  ( ServerCertificateMetadata (..)
  -- * Smart constructor
  , mkServerCertificateMetadata
  -- * Lenses
  , scmPath
  , scmServerCertificateName
  , scmServerCertificateId
  , scmArn
  , scmExpiration
  , scmUploadDate
  ) where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.IAM.Types.IdType as Types
import qualified Network.AWS.IAM.Types.Path as Types
import qualified Network.AWS.IAM.Types.ServerCertificateName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a server certificate without its certificate body, certificate chain, and private key.
--
-- This data type is used as a response element in the 'UploadServerCertificate' and 'ListServerCertificates' operations. 
--
-- /See:/ 'mkServerCertificateMetadata' smart constructor.
data ServerCertificateMetadata = ServerCertificateMetadata'
  { path :: Types.Path
    -- ^ The path to the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
  , serverCertificateName :: Types.ServerCertificateName
    -- ^ The name that identifies the server certificate.
  , serverCertificateId :: Types.IdType
    -- ^ The stable and unique string identifying the server certificate. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
  , arn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) specifying the server certificate. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
  , expiration :: Core.Maybe Core.UTCTime
    -- ^ The date on which the certificate is set to expire.
  , uploadDate :: Core.Maybe Core.UTCTime
    -- ^ The date when the server certificate was uploaded.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ServerCertificateMetadata' value with any optional fields omitted.
mkServerCertificateMetadata
    :: Types.Path -- ^ 'path'
    -> Types.ServerCertificateName -- ^ 'serverCertificateName'
    -> Types.IdType -- ^ 'serverCertificateId'
    -> Types.Arn -- ^ 'arn'
    -> ServerCertificateMetadata
mkServerCertificateMetadata path serverCertificateName
  serverCertificateId arn
  = ServerCertificateMetadata'{path, serverCertificateName,
                               serverCertificateId, arn, expiration = Core.Nothing,
                               uploadDate = Core.Nothing}

-- | The path to the server certificate. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmPath :: Lens.Lens' ServerCertificateMetadata Types.Path
scmPath = Lens.field @"path"
{-# INLINEABLE scmPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The name that identifies the server certificate.
--
-- /Note:/ Consider using 'serverCertificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmServerCertificateName :: Lens.Lens' ServerCertificateMetadata Types.ServerCertificateName
scmServerCertificateName = Lens.field @"serverCertificateName"
{-# INLINEABLE scmServerCertificateName #-}
{-# DEPRECATED serverCertificateName "Use generic-lens or generic-optics with 'serverCertificateName' instead"  #-}

-- | The stable and unique string identifying the server certificate. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
--
-- /Note:/ Consider using 'serverCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmServerCertificateId :: Lens.Lens' ServerCertificateMetadata Types.IdType
scmServerCertificateId = Lens.field @"serverCertificateId"
{-# INLINEABLE scmServerCertificateId #-}
{-# DEPRECATED serverCertificateId "Use generic-lens or generic-optics with 'serverCertificateId' instead"  #-}

-- | The Amazon Resource Name (ARN) specifying the server certificate. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmArn :: Lens.Lens' ServerCertificateMetadata Types.Arn
scmArn = Lens.field @"arn"
{-# INLINEABLE scmArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date on which the certificate is set to expire.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmExpiration :: Lens.Lens' ServerCertificateMetadata (Core.Maybe Core.UTCTime)
scmExpiration = Lens.field @"expiration"
{-# INLINEABLE scmExpiration #-}
{-# DEPRECATED expiration "Use generic-lens or generic-optics with 'expiration' instead"  #-}

-- | The date when the server certificate was uploaded.
--
-- /Note:/ Consider using 'uploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmUploadDate :: Lens.Lens' ServerCertificateMetadata (Core.Maybe Core.UTCTime)
scmUploadDate = Lens.field @"uploadDate"
{-# INLINEABLE scmUploadDate #-}
{-# DEPRECATED uploadDate "Use generic-lens or generic-optics with 'uploadDate' instead"  #-}

instance Core.FromXML ServerCertificateMetadata where
        parseXML x
          = ServerCertificateMetadata' Core.<$>
              (x Core..@ "Path") Core.<*> x Core..@ "ServerCertificateName"
                Core.<*> x Core..@ "ServerCertificateId"
                Core.<*> x Core..@ "Arn"
                Core.<*> x Core..@? "Expiration"
                Core.<*> x Core..@? "UploadDate"
