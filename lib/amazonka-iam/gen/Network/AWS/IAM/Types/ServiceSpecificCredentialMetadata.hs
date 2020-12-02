{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata where

import Network.AWS.IAM.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains additional details about a service-specific credential.
--
--
--
-- /See:/ 'serviceSpecificCredentialMetadata' smart constructor.
data ServiceSpecificCredentialMetadata = ServiceSpecificCredentialMetadata'
  { _sscmUserName ::
      !Text,
    _sscmStatus ::
      !StatusType,
    _sscmServiceUserName ::
      !Text,
    _sscmCreateDate ::
      !ISO8601,
    _sscmServiceSpecificCredentialId ::
      !Text,
    _sscmServiceName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceSpecificCredentialMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sscmUserName' - The name of the IAM user associated with the service-specific credential.
--
-- * 'sscmStatus' - The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
--
-- * 'sscmServiceUserName' - The generated user name for the service-specific credential.
--
-- * 'sscmCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
--
-- * 'sscmServiceSpecificCredentialId' - The unique identifier for the service-specific credential.
--
-- * 'sscmServiceName' - The name of the service associated with the service-specific credential.
serviceSpecificCredentialMetadata ::
  -- | 'sscmUserName'
  Text ->
  -- | 'sscmStatus'
  StatusType ->
  -- | 'sscmServiceUserName'
  Text ->
  -- | 'sscmCreateDate'
  UTCTime ->
  -- | 'sscmServiceSpecificCredentialId'
  Text ->
  -- | 'sscmServiceName'
  Text ->
  ServiceSpecificCredentialMetadata
serviceSpecificCredentialMetadata
  pUserName_
  pStatus_
  pServiceUserName_
  pCreateDate_
  pServiceSpecificCredentialId_
  pServiceName_ =
    ServiceSpecificCredentialMetadata'
      { _sscmUserName = pUserName_,
        _sscmStatus = pStatus_,
        _sscmServiceUserName = pServiceUserName_,
        _sscmCreateDate = _Time # pCreateDate_,
        _sscmServiceSpecificCredentialId =
          pServiceSpecificCredentialId_,
        _sscmServiceName = pServiceName_
      }

-- | The name of the IAM user associated with the service-specific credential.
sscmUserName :: Lens' ServiceSpecificCredentialMetadata Text
sscmUserName = lens _sscmUserName (\s a -> s {_sscmUserName = a})

-- | The status of the service-specific credential. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
sscmStatus :: Lens' ServiceSpecificCredentialMetadata StatusType
sscmStatus = lens _sscmStatus (\s a -> s {_sscmStatus = a})

-- | The generated user name for the service-specific credential.
sscmServiceUserName :: Lens' ServiceSpecificCredentialMetadata Text
sscmServiceUserName = lens _sscmServiceUserName (\s a -> s {_sscmServiceUserName = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the service-specific credential were created.
sscmCreateDate :: Lens' ServiceSpecificCredentialMetadata UTCTime
sscmCreateDate = lens _sscmCreateDate (\s a -> s {_sscmCreateDate = a}) . _Time

-- | The unique identifier for the service-specific credential.
sscmServiceSpecificCredentialId :: Lens' ServiceSpecificCredentialMetadata Text
sscmServiceSpecificCredentialId = lens _sscmServiceSpecificCredentialId (\s a -> s {_sscmServiceSpecificCredentialId = a})

-- | The name of the service associated with the service-specific credential.
sscmServiceName :: Lens' ServiceSpecificCredentialMetadata Text
sscmServiceName = lens _sscmServiceName (\s a -> s {_sscmServiceName = a})

instance FromXML ServiceSpecificCredentialMetadata where
  parseXML x =
    ServiceSpecificCredentialMetadata'
      <$> (x .@ "UserName")
      <*> (x .@ "Status")
      <*> (x .@ "ServiceUserName")
      <*> (x .@ "CreateDate")
      <*> (x .@ "ServiceSpecificCredentialId")
      <*> (x .@ "ServiceName")

instance Hashable ServiceSpecificCredentialMetadata

instance NFData ServiceSpecificCredentialMetadata
