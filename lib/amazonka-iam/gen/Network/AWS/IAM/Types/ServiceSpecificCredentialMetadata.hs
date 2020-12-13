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
    sscmStatus,
    sscmCreateDate,
    sscmUserName,
    sscmServiceName,
    sscmServiceSpecificCredentialId,
    sscmServiceUserName,
  )
where

import Network.AWS.IAM.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains additional details about a service-specific credential.
--
-- /See:/ 'mkServiceSpecificCredentialMetadata' smart constructor.
data ServiceSpecificCredentialMetadata = ServiceSpecificCredentialMetadata'
  { -- | The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
    status :: StatusType,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
    createDate :: Lude.DateTime,
    -- | The name of the IAM user associated with the service-specific credential.
    userName :: Lude.Text,
    -- | The name of the service associated with the service-specific credential.
    serviceName :: Lude.Text,
    -- | The unique identifier for the service-specific credential.
    serviceSpecificCredentialId :: Lude.Text,
    -- | The generated user name for the service-specific credential.
    serviceUserName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceSpecificCredentialMetadata' with the minimum fields required to make a request.
--
-- * 'status' - The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
-- * 'userName' - The name of the IAM user associated with the service-specific credential.
-- * 'serviceName' - The name of the service associated with the service-specific credential.
-- * 'serviceSpecificCredentialId' - The unique identifier for the service-specific credential.
-- * 'serviceUserName' - The generated user name for the service-specific credential.
mkServiceSpecificCredentialMetadata ::
  -- | 'status'
  StatusType ->
  -- | 'createDate'
  Lude.DateTime ->
  -- | 'userName'
  Lude.Text ->
  -- | 'serviceName'
  Lude.Text ->
  -- | 'serviceSpecificCredentialId'
  Lude.Text ->
  -- | 'serviceUserName'
  Lude.Text ->
  ServiceSpecificCredentialMetadata
mkServiceSpecificCredentialMetadata
  pStatus_
  pCreateDate_
  pUserName_
  pServiceName_
  pServiceSpecificCredentialId_
  pServiceUserName_ =
    ServiceSpecificCredentialMetadata'
      { status = pStatus_,
        createDate = pCreateDate_,
        userName = pUserName_,
        serviceName = pServiceName_,
        serviceSpecificCredentialId = pServiceSpecificCredentialId_,
        serviceUserName = pServiceUserName_
      }

-- | The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmStatus :: Lens.Lens' ServiceSpecificCredentialMetadata StatusType
sscmStatus = Lens.lens (status :: ServiceSpecificCredentialMetadata -> StatusType) (\s a -> s {status = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmCreateDate :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.DateTime
sscmCreateDate = Lens.lens (createDate :: ServiceSpecificCredentialMetadata -> Lude.DateTime) (\s a -> s {createDate = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The name of the IAM user associated with the service-specific credential.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmUserName :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.Text
sscmUserName = Lens.lens (userName :: ServiceSpecificCredentialMetadata -> Lude.Text) (\s a -> s {userName = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The name of the service associated with the service-specific credential.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmServiceName :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.Text
sscmServiceName = Lens.lens (serviceName :: ServiceSpecificCredentialMetadata -> Lude.Text) (\s a -> s {serviceName = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The unique identifier for the service-specific credential.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmServiceSpecificCredentialId :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.Text
sscmServiceSpecificCredentialId = Lens.lens (serviceSpecificCredentialId :: ServiceSpecificCredentialMetadata -> Lude.Text) (\s a -> s {serviceSpecificCredentialId = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmServiceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead." #-}

-- | The generated user name for the service-specific credential.
--
-- /Note:/ Consider using 'serviceUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmServiceUserName :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.Text
sscmServiceUserName = Lens.lens (serviceUserName :: ServiceSpecificCredentialMetadata -> Lude.Text) (\s a -> s {serviceUserName = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmServiceUserName "Use generic-lens or generic-optics with 'serviceUserName' instead." #-}

instance Lude.FromXML ServiceSpecificCredentialMetadata where
  parseXML x =
    ServiceSpecificCredentialMetadata'
      Lude.<$> (x Lude..@ "Status")
      Lude.<*> (x Lude..@ "CreateDate")
      Lude.<*> (x Lude..@ "UserName")
      Lude.<*> (x Lude..@ "ServiceName")
      Lude.<*> (x Lude..@ "ServiceSpecificCredentialId")
      Lude.<*> (x Lude..@ "ServiceUserName")
