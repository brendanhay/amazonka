{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata where

import Network.AWS.IAM.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains additional details about a service-specific credential.
--
-- /See:/ 'newServiceSpecificCredentialMetadata' smart constructor.
data ServiceSpecificCredentialMetadata = ServiceSpecificCredentialMetadata'
  { -- | The name of the IAM user associated with the service-specific
    -- credential.
    userName :: Prelude.Text,
    -- | The status of the service-specific credential. @Active@ means that the
    -- key is valid for API calls, while @Inactive@ means it is not.
    status :: StatusType,
    -- | The generated user name for the service-specific credential.
    serviceUserName :: Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- service-specific credential were created.
    createDate :: Prelude.ISO8601,
    -- | The unique identifier for the service-specific credential.
    serviceSpecificCredentialId :: Prelude.Text,
    -- | The name of the service associated with the service-specific credential.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceSpecificCredentialMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'serviceSpecificCredentialMetadata_userName' - The name of the IAM user associated with the service-specific
-- credential.
--
-- 'status', 'serviceSpecificCredentialMetadata_status' - The status of the service-specific credential. @Active@ means that the
-- key is valid for API calls, while @Inactive@ means it is not.
--
-- 'serviceUserName', 'serviceSpecificCredentialMetadata_serviceUserName' - The generated user name for the service-specific credential.
--
-- 'createDate', 'serviceSpecificCredentialMetadata_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- service-specific credential were created.
--
-- 'serviceSpecificCredentialId', 'serviceSpecificCredentialMetadata_serviceSpecificCredentialId' - The unique identifier for the service-specific credential.
--
-- 'serviceName', 'serviceSpecificCredentialMetadata_serviceName' - The name of the service associated with the service-specific credential.
newServiceSpecificCredentialMetadata ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'status'
  StatusType ->
  -- | 'serviceUserName'
  Prelude.Text ->
  -- | 'createDate'
  Prelude.UTCTime ->
  -- | 'serviceSpecificCredentialId'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  ServiceSpecificCredentialMetadata
newServiceSpecificCredentialMetadata
  pUserName_
  pStatus_
  pServiceUserName_
  pCreateDate_
  pServiceSpecificCredentialId_
  pServiceName_ =
    ServiceSpecificCredentialMetadata'
      { userName =
          pUserName_,
        status = pStatus_,
        serviceUserName = pServiceUserName_,
        createDate =
          Prelude._Time Lens.# pCreateDate_,
        serviceSpecificCredentialId =
          pServiceSpecificCredentialId_,
        serviceName = pServiceName_
      }

-- | The name of the IAM user associated with the service-specific
-- credential.
serviceSpecificCredentialMetadata_userName :: Lens.Lens' ServiceSpecificCredentialMetadata Prelude.Text
serviceSpecificCredentialMetadata_userName = Lens.lens (\ServiceSpecificCredentialMetadata' {userName} -> userName) (\s@ServiceSpecificCredentialMetadata' {} a -> s {userName = a} :: ServiceSpecificCredentialMetadata)

-- | The status of the service-specific credential. @Active@ means that the
-- key is valid for API calls, while @Inactive@ means it is not.
serviceSpecificCredentialMetadata_status :: Lens.Lens' ServiceSpecificCredentialMetadata StatusType
serviceSpecificCredentialMetadata_status = Lens.lens (\ServiceSpecificCredentialMetadata' {status} -> status) (\s@ServiceSpecificCredentialMetadata' {} a -> s {status = a} :: ServiceSpecificCredentialMetadata)

-- | The generated user name for the service-specific credential.
serviceSpecificCredentialMetadata_serviceUserName :: Lens.Lens' ServiceSpecificCredentialMetadata Prelude.Text
serviceSpecificCredentialMetadata_serviceUserName = Lens.lens (\ServiceSpecificCredentialMetadata' {serviceUserName} -> serviceUserName) (\s@ServiceSpecificCredentialMetadata' {} a -> s {serviceUserName = a} :: ServiceSpecificCredentialMetadata)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- service-specific credential were created.
serviceSpecificCredentialMetadata_createDate :: Lens.Lens' ServiceSpecificCredentialMetadata Prelude.UTCTime
serviceSpecificCredentialMetadata_createDate = Lens.lens (\ServiceSpecificCredentialMetadata' {createDate} -> createDate) (\s@ServiceSpecificCredentialMetadata' {} a -> s {createDate = a} :: ServiceSpecificCredentialMetadata) Prelude.. Prelude._Time

-- | The unique identifier for the service-specific credential.
serviceSpecificCredentialMetadata_serviceSpecificCredentialId :: Lens.Lens' ServiceSpecificCredentialMetadata Prelude.Text
serviceSpecificCredentialMetadata_serviceSpecificCredentialId = Lens.lens (\ServiceSpecificCredentialMetadata' {serviceSpecificCredentialId} -> serviceSpecificCredentialId) (\s@ServiceSpecificCredentialMetadata' {} a -> s {serviceSpecificCredentialId = a} :: ServiceSpecificCredentialMetadata)

-- | The name of the service associated with the service-specific credential.
serviceSpecificCredentialMetadata_serviceName :: Lens.Lens' ServiceSpecificCredentialMetadata Prelude.Text
serviceSpecificCredentialMetadata_serviceName = Lens.lens (\ServiceSpecificCredentialMetadata' {serviceName} -> serviceName) (\s@ServiceSpecificCredentialMetadata' {} a -> s {serviceName = a} :: ServiceSpecificCredentialMetadata)

instance
  Prelude.FromXML
    ServiceSpecificCredentialMetadata
  where
  parseXML x =
    ServiceSpecificCredentialMetadata'
      Prelude.<$> (x Prelude..@ "UserName")
      Prelude.<*> (x Prelude..@ "Status")
      Prelude.<*> (x Prelude..@ "ServiceUserName")
      Prelude.<*> (x Prelude..@ "CreateDate")
      Prelude.<*> (x Prelude..@ "ServiceSpecificCredentialId")
      Prelude.<*> (x Prelude..@ "ServiceName")

instance
  Prelude.Hashable
    ServiceSpecificCredentialMetadata

instance
  Prelude.NFData
    ServiceSpecificCredentialMetadata
