{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Operation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Operation where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.OperationStatus
import Network.AWS.Lightsail.Types.OperationType
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Prelude

-- | Describes the API operation.
--
--
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
  { _opeStatus :: !(Maybe OperationStatus),
    _opeOperationDetails :: !(Maybe Text),
    _opeResourceType :: !(Maybe ResourceType),
    _opeCreatedAt :: !(Maybe POSIX),
    _opeResourceName :: !(Maybe Text),
    _opeLocation :: !(Maybe ResourceLocation),
    _opeStatusChangedAt :: !(Maybe POSIX),
    _opeErrorDetails :: !(Maybe Text),
    _opeErrorCode :: !(Maybe Text),
    _opeId :: !(Maybe Text),
    _opeOperationType :: !(Maybe OperationType),
    _opeIsTerminal :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'opeStatus' - The status of the operation.
--
-- * 'opeOperationDetails' - Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
--
-- * 'opeResourceType' - The resource type.
--
-- * 'opeCreatedAt' - The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
--
-- * 'opeResourceName' - The resource name.
--
-- * 'opeLocation' - The AWS Region and Availability Zone.
--
-- * 'opeStatusChangedAt' - The timestamp when the status was changed (e.g., @1479816991.349@ ).
--
-- * 'opeErrorDetails' - The error details.
--
-- * 'opeErrorCode' - The error code.
--
-- * 'opeId' - The ID of the operation.
--
-- * 'opeOperationType' - The type of operation.
--
-- * 'opeIsTerminal' - A Boolean value indicating whether the operation is terminal.
operation ::
  Operation
operation =
  Operation'
    { _opeStatus = Nothing,
      _opeOperationDetails = Nothing,
      _opeResourceType = Nothing,
      _opeCreatedAt = Nothing,
      _opeResourceName = Nothing,
      _opeLocation = Nothing,
      _opeStatusChangedAt = Nothing,
      _opeErrorDetails = Nothing,
      _opeErrorCode = Nothing,
      _opeId = Nothing,
      _opeOperationType = Nothing,
      _opeIsTerminal = Nothing
    }

-- | The status of the operation.
opeStatus :: Lens' Operation (Maybe OperationStatus)
opeStatus = lens _opeStatus (\s a -> s {_opeStatus = a})

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
opeOperationDetails :: Lens' Operation (Maybe Text)
opeOperationDetails = lens _opeOperationDetails (\s a -> s {_opeOperationDetails = a})

-- | The resource type.
opeResourceType :: Lens' Operation (Maybe ResourceType)
opeResourceType = lens _opeResourceType (\s a -> s {_opeResourceType = a})

-- | The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
opeCreatedAt :: Lens' Operation (Maybe UTCTime)
opeCreatedAt = lens _opeCreatedAt (\s a -> s {_opeCreatedAt = a}) . mapping _Time

-- | The resource name.
opeResourceName :: Lens' Operation (Maybe Text)
opeResourceName = lens _opeResourceName (\s a -> s {_opeResourceName = a})

-- | The AWS Region and Availability Zone.
opeLocation :: Lens' Operation (Maybe ResourceLocation)
opeLocation = lens _opeLocation (\s a -> s {_opeLocation = a})

-- | The timestamp when the status was changed (e.g., @1479816991.349@ ).
opeStatusChangedAt :: Lens' Operation (Maybe UTCTime)
opeStatusChangedAt = lens _opeStatusChangedAt (\s a -> s {_opeStatusChangedAt = a}) . mapping _Time

-- | The error details.
opeErrorDetails :: Lens' Operation (Maybe Text)
opeErrorDetails = lens _opeErrorDetails (\s a -> s {_opeErrorDetails = a})

-- | The error code.
opeErrorCode :: Lens' Operation (Maybe Text)
opeErrorCode = lens _opeErrorCode (\s a -> s {_opeErrorCode = a})

-- | The ID of the operation.
opeId :: Lens' Operation (Maybe Text)
opeId = lens _opeId (\s a -> s {_opeId = a})

-- | The type of operation.
opeOperationType :: Lens' Operation (Maybe OperationType)
opeOperationType = lens _opeOperationType (\s a -> s {_opeOperationType = a})

-- | A Boolean value indicating whether the operation is terminal.
opeIsTerminal :: Lens' Operation (Maybe Bool)
opeIsTerminal = lens _opeIsTerminal (\s a -> s {_opeIsTerminal = a})

instance FromJSON Operation where
  parseJSON =
    withObject
      "Operation"
      ( \x ->
          Operation'
            <$> (x .:? "status")
            <*> (x .:? "operationDetails")
            <*> (x .:? "resourceType")
            <*> (x .:? "createdAt")
            <*> (x .:? "resourceName")
            <*> (x .:? "location")
            <*> (x .:? "statusChangedAt")
            <*> (x .:? "errorDetails")
            <*> (x .:? "errorCode")
            <*> (x .:? "id")
            <*> (x .:? "operationType")
            <*> (x .:? "isTerminal")
      )

instance Hashable Operation

instance NFData Operation
