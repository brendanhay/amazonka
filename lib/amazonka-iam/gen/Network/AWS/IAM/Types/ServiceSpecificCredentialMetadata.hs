{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata
  ( ServiceSpecificCredentialMetadata (..),

    -- * Smart constructor
    mkServiceSpecificCredentialMetadata,

    -- * Lenses
    sscmUserName,
    sscmStatus,
    sscmServiceUserName,
    sscmCreateDate,
    sscmServiceSpecificCredentialId,
    sscmServiceName,
  )
where

import qualified Network.AWS.IAM.Types.ServiceName as Types
import qualified Network.AWS.IAM.Types.ServiceSpecificCredentialId as Types
import qualified Network.AWS.IAM.Types.ServiceUserName as Types
import qualified Network.AWS.IAM.Types.StatusType as Types
import qualified Network.AWS.IAM.Types.UserName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains additional details about a service-specific credential.
--
-- /See:/ 'mkServiceSpecificCredentialMetadata' smart constructor.
data ServiceSpecificCredentialMetadata = ServiceSpecificCredentialMetadata'
  { -- | The name of the IAM user associated with the service-specific credential.
    userName :: Types.UserName,
    -- | The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
    status :: Types.StatusType,
    -- | The generated user name for the service-specific credential.
    serviceUserName :: Types.ServiceUserName,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
    createDate :: Core.UTCTime,
    -- | The unique identifier for the service-specific credential.
    serviceSpecificCredentialId :: Types.ServiceSpecificCredentialId,
    -- | The name of the service associated with the service-specific credential.
    serviceName :: Types.ServiceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ServiceSpecificCredentialMetadata' value with any optional fields omitted.
mkServiceSpecificCredentialMetadata ::
  -- | 'userName'
  Types.UserName ->
  -- | 'status'
  Types.StatusType ->
  -- | 'serviceUserName'
  Types.ServiceUserName ->
  -- | 'createDate'
  Core.UTCTime ->
  -- | 'serviceSpecificCredentialId'
  Types.ServiceSpecificCredentialId ->
  -- | 'serviceName'
  Types.ServiceName ->
  ServiceSpecificCredentialMetadata
mkServiceSpecificCredentialMetadata
  userName
  status
  serviceUserName
  createDate
  serviceSpecificCredentialId
  serviceName =
    ServiceSpecificCredentialMetadata'
      { userName,
        status,
        serviceUserName,
        createDate,
        serviceSpecificCredentialId,
        serviceName
      }

-- | The name of the IAM user associated with the service-specific credential.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmUserName :: Lens.Lens' ServiceSpecificCredentialMetadata Types.UserName
sscmUserName = Lens.field @"userName"
{-# DEPRECATED sscmUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmStatus :: Lens.Lens' ServiceSpecificCredentialMetadata Types.StatusType
sscmStatus = Lens.field @"status"
{-# DEPRECATED sscmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The generated user name for the service-specific credential.
--
-- /Note:/ Consider using 'serviceUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmServiceUserName :: Lens.Lens' ServiceSpecificCredentialMetadata Types.ServiceUserName
sscmServiceUserName = Lens.field @"serviceUserName"
{-# DEPRECATED sscmServiceUserName "Use generic-lens or generic-optics with 'serviceUserName' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmCreateDate :: Lens.Lens' ServiceSpecificCredentialMetadata Core.UTCTime
sscmCreateDate = Lens.field @"createDate"
{-# DEPRECATED sscmCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The unique identifier for the service-specific credential.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmServiceSpecificCredentialId :: Lens.Lens' ServiceSpecificCredentialMetadata Types.ServiceSpecificCredentialId
sscmServiceSpecificCredentialId = Lens.field @"serviceSpecificCredentialId"
{-# DEPRECATED sscmServiceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead." #-}

-- | The name of the service associated with the service-specific credential.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmServiceName :: Lens.Lens' ServiceSpecificCredentialMetadata Types.ServiceName
sscmServiceName = Lens.field @"serviceName"
{-# DEPRECATED sscmServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Core.FromXML ServiceSpecificCredentialMetadata where
  parseXML x =
    ServiceSpecificCredentialMetadata'
      Core.<$> (x Core..@ "UserName")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@ "ServiceUserName")
      Core.<*> (x Core..@ "CreateDate")
      Core.<*> (x Core..@ "ServiceSpecificCredentialId")
      Core.<*> (x Core..@ "ServiceName")
