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

import Network.AWS.IAM.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains additional details about a service-specific credential.
--
-- /See:/ 'mkServiceSpecificCredentialMetadata' smart constructor.
data ServiceSpecificCredentialMetadata = ServiceSpecificCredentialMetadata'
  { userName ::
      Lude.Text,
    status :: StatusType,
    serviceUserName ::
      Lude.Text,
    createDate ::
      Lude.DateTime,
    serviceSpecificCredentialId ::
      Lude.Text,
    serviceName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceSpecificCredentialMetadata' with the minimum fields required to make a request.
--
-- * 'createDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
-- * 'serviceName' - The name of the service associated with the service-specific credential.
-- * 'serviceSpecificCredentialId' - The unique identifier for the service-specific credential.
-- * 'serviceUserName' - The generated user name for the service-specific credential.
-- * 'status' - The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
-- * 'userName' - The name of the IAM user associated with the service-specific credential.
mkServiceSpecificCredentialMetadata ::
  -- | 'userName'
  Lude.Text ->
  -- | 'status'
  StatusType ->
  -- | 'serviceUserName'
  Lude.Text ->
  -- | 'createDate'
  Lude.DateTime ->
  -- | 'serviceSpecificCredentialId'
  Lude.Text ->
  -- | 'serviceName'
  Lude.Text ->
  ServiceSpecificCredentialMetadata
mkServiceSpecificCredentialMetadata
  pUserName_
  pStatus_
  pServiceUserName_
  pCreateDate_
  pServiceSpecificCredentialId_
  pServiceName_ =
    ServiceSpecificCredentialMetadata'
      { userName = pUserName_,
        status = pStatus_,
        serviceUserName = pServiceUserName_,
        createDate = pCreateDate_,
        serviceSpecificCredentialId = pServiceSpecificCredentialId_,
        serviceName = pServiceName_
      }

-- | The name of the IAM user associated with the service-specific credential.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmUserName :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.Text
sscmUserName = Lens.lens (userName :: ServiceSpecificCredentialMetadata -> Lude.Text) (\s a -> s {userName = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmStatus :: Lens.Lens' ServiceSpecificCredentialMetadata StatusType
sscmStatus = Lens.lens (status :: ServiceSpecificCredentialMetadata -> StatusType) (\s a -> s {status = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The generated user name for the service-specific credential.
--
-- /Note:/ Consider using 'serviceUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmServiceUserName :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.Text
sscmServiceUserName = Lens.lens (serviceUserName :: ServiceSpecificCredentialMetadata -> Lude.Text) (\s a -> s {serviceUserName = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmServiceUserName "Use generic-lens or generic-optics with 'serviceUserName' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmCreateDate :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.DateTime
sscmCreateDate = Lens.lens (createDate :: ServiceSpecificCredentialMetadata -> Lude.DateTime) (\s a -> s {createDate = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The unique identifier for the service-specific credential.
--
-- /Note:/ Consider using 'serviceSpecificCredentialId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmServiceSpecificCredentialId :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.Text
sscmServiceSpecificCredentialId = Lens.lens (serviceSpecificCredentialId :: ServiceSpecificCredentialMetadata -> Lude.Text) (\s a -> s {serviceSpecificCredentialId = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmServiceSpecificCredentialId "Use generic-lens or generic-optics with 'serviceSpecificCredentialId' instead." #-}

-- | The name of the service associated with the service-specific credential.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscmServiceName :: Lens.Lens' ServiceSpecificCredentialMetadata Lude.Text
sscmServiceName = Lens.lens (serviceName :: ServiceSpecificCredentialMetadata -> Lude.Text) (\s a -> s {serviceName = a} :: ServiceSpecificCredentialMetadata)
{-# DEPRECATED sscmServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

instance Lude.FromXML ServiceSpecificCredentialMetadata where
  parseXML x =
    ServiceSpecificCredentialMetadata'
      Lude.<$> (x Lude..@ "UserName")
      Lude.<*> (x Lude..@ "Status")
      Lude.<*> (x Lude..@ "ServiceUserName")
      Lude.<*> (x Lude..@ "CreateDate")
      Lude.<*> (x Lude..@ "ServiceSpecificCredentialId")
      Lude.<*> (x Lude..@ "ServiceName")
