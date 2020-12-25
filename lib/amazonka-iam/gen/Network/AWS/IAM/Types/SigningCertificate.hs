{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SigningCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SigningCertificate
  ( SigningCertificate (..),

    -- * Smart constructor
    mkSigningCertificate,

    -- * Lenses
    scUserName,
    scCertificateId,
    scCertificateBody,
    scStatus,
    scUploadDate,
  )
where

import qualified Network.AWS.IAM.Types.CertificateBodyType as Types
import qualified Network.AWS.IAM.Types.CertificateIdType as Types
import qualified Network.AWS.IAM.Types.StatusType as Types
import qualified Network.AWS.IAM.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an X.509 signing certificate.
--
-- This data type is used as a response element in the 'UploadSigningCertificate' and 'ListSigningCertificates' operations.
--
-- /See:/ 'mkSigningCertificate' smart constructor.
data SigningCertificate = SigningCertificate'
  { -- | The name of the user the signing certificate is associated with.
    userName :: Types.UserName,
    -- | The ID for the signing certificate.
    certificateId :: Types.CertificateIdType,
    -- | The contents of the signing certificate.
    certificateBody :: Types.CertificateBodyType,
    -- | The status of the signing certificate. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
    status :: Types.StatusType,
    -- | The date when the signing certificate was uploaded.
    uploadDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SigningCertificate' value with any optional fields omitted.
mkSigningCertificate ::
  -- | 'userName'
  Types.UserName ->
  -- | 'certificateId'
  Types.CertificateIdType ->
  -- | 'certificateBody'
  Types.CertificateBodyType ->
  -- | 'status'
  Types.StatusType ->
  SigningCertificate
mkSigningCertificate userName certificateId certificateBody status =
  SigningCertificate'
    { userName,
      certificateId,
      certificateBody,
      status,
      uploadDate = Core.Nothing
    }

-- | The name of the user the signing certificate is associated with.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scUserName :: Lens.Lens' SigningCertificate Types.UserName
scUserName = Lens.field @"userName"
{-# DEPRECATED scUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The ID for the signing certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scCertificateId :: Lens.Lens' SigningCertificate Types.CertificateIdType
scCertificateId = Lens.field @"certificateId"
{-# DEPRECATED scCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The contents of the signing certificate.
--
-- /Note:/ Consider using 'certificateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scCertificateBody :: Lens.Lens' SigningCertificate Types.CertificateBodyType
scCertificateBody = Lens.field @"certificateBody"
{-# DEPRECATED scCertificateBody "Use generic-lens or generic-optics with 'certificateBody' instead." #-}

-- | The status of the signing certificate. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scStatus :: Lens.Lens' SigningCertificate Types.StatusType
scStatus = Lens.field @"status"
{-# DEPRECATED scStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date when the signing certificate was uploaded.
--
-- /Note:/ Consider using 'uploadDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scUploadDate :: Lens.Lens' SigningCertificate (Core.Maybe Core.UTCTime)
scUploadDate = Lens.field @"uploadDate"
{-# DEPRECATED scUploadDate "Use generic-lens or generic-optics with 'uploadDate' instead." #-}

instance Core.FromXML SigningCertificate where
  parseXML x =
    SigningCertificate'
      Core.<$> (x Core..@ "UserName")
      Core.<*> (x Core..@ "CertificateId")
      Core.<*> (x Core..@ "CertificateBody")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@? "UploadDate")
