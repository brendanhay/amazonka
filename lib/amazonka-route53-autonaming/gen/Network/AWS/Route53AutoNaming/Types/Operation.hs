{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.Operation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.Operation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.OperationStatus
import Network.AWS.Route53AutoNaming.Types.OperationTargetType
import Network.AWS.Route53AutoNaming.Types.OperationType

-- | A complex type that contains information about a specified operation.
--
--
--
-- /See:/ 'operation' smart constructor.
data Operation = Operation'
  { _oStatus :: !(Maybe OperationStatus),
    _oUpdateDate :: !(Maybe POSIX),
    _oCreateDate :: !(Maybe POSIX),
    _oTargets :: !(Maybe (Map OperationTargetType (Text))),
    _oErrorCode :: !(Maybe Text),
    _oId :: !(Maybe Text),
    _oType :: !(Maybe OperationType),
    _oErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oStatus' - The status of the operation. Values include the following:     * __SUBMITTED__ : This is the initial state immediately after you submit a request.     * __PENDING__ : AWS Cloud Map is performing the operation.     * __SUCCESS__ : The operation succeeded.     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
--
-- * 'oUpdateDate' - The date and time that the value of @Status@ changed to the current value, in Unix date/time format and Coordinated Universal Time (UTC). The value of @UpdateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'oCreateDate' - The date and time that the request was submitted, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- * 'oTargets' - The name of the target entity that is associated with the operation:     * __NAMESPACE__ : The namespace ID is returned in the @ResourceId@ property.     * __SERVICE__ : The service ID is returned in the @ResourceId@ property.     * __INSTANCE__ : The instance ID is returned in the @ResourceId@ property.
--
-- * 'oErrorCode' - The code associated with @ErrorMessage@ . Values for @ErrorCode@ include the following:     * @ACCESS_DENIED@      * @CANNOT_CREATE_HOSTED_ZONE@      * @EXPIRED_TOKEN@      * @HOSTED_ZONE_NOT_FOUND@      * @INTERNAL_FAILURE@      * @INVALID_CHANGE_BATCH@      * @THROTTLED_REQUEST@
--
-- * 'oId' - The ID of the operation that you want to get information about.
--
-- * 'oType' - The name of the operation that is associated with the specified ID.
--
-- * 'oErrorMessage' - If the value of @Status@ is @FAIL@ , the reason that the operation failed.
operation ::
  Operation
operation =
  Operation'
    { _oStatus = Nothing,
      _oUpdateDate = Nothing,
      _oCreateDate = Nothing,
      _oTargets = Nothing,
      _oErrorCode = Nothing,
      _oId = Nothing,
      _oType = Nothing,
      _oErrorMessage = Nothing
    }

-- | The status of the operation. Values include the following:     * __SUBMITTED__ : This is the initial state immediately after you submit a request.     * __PENDING__ : AWS Cloud Map is performing the operation.     * __SUCCESS__ : The operation succeeded.     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
oStatus :: Lens' Operation (Maybe OperationStatus)
oStatus = lens _oStatus (\s a -> s {_oStatus = a})

-- | The date and time that the value of @Status@ changed to the current value, in Unix date/time format and Coordinated Universal Time (UTC). The value of @UpdateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
oUpdateDate :: Lens' Operation (Maybe UTCTime)
oUpdateDate = lens _oUpdateDate (\s a -> s {_oUpdateDate = a}) . mapping _Time

-- | The date and time that the request was submitted, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
oCreateDate :: Lens' Operation (Maybe UTCTime)
oCreateDate = lens _oCreateDate (\s a -> s {_oCreateDate = a}) . mapping _Time

-- | The name of the target entity that is associated with the operation:     * __NAMESPACE__ : The namespace ID is returned in the @ResourceId@ property.     * __SERVICE__ : The service ID is returned in the @ResourceId@ property.     * __INSTANCE__ : The instance ID is returned in the @ResourceId@ property.
oTargets :: Lens' Operation (HashMap OperationTargetType (Text))
oTargets = lens _oTargets (\s a -> s {_oTargets = a}) . _Default . _Map

-- | The code associated with @ErrorMessage@ . Values for @ErrorCode@ include the following:     * @ACCESS_DENIED@      * @CANNOT_CREATE_HOSTED_ZONE@      * @EXPIRED_TOKEN@      * @HOSTED_ZONE_NOT_FOUND@      * @INTERNAL_FAILURE@      * @INVALID_CHANGE_BATCH@      * @THROTTLED_REQUEST@
oErrorCode :: Lens' Operation (Maybe Text)
oErrorCode = lens _oErrorCode (\s a -> s {_oErrorCode = a})

-- | The ID of the operation that you want to get information about.
oId :: Lens' Operation (Maybe Text)
oId = lens _oId (\s a -> s {_oId = a})

-- | The name of the operation that is associated with the specified ID.
oType :: Lens' Operation (Maybe OperationType)
oType = lens _oType (\s a -> s {_oType = a})

-- | If the value of @Status@ is @FAIL@ , the reason that the operation failed.
oErrorMessage :: Lens' Operation (Maybe Text)
oErrorMessage = lens _oErrorMessage (\s a -> s {_oErrorMessage = a})

instance FromJSON Operation where
  parseJSON =
    withObject
      "Operation"
      ( \x ->
          Operation'
            <$> (x .:? "Status")
            <*> (x .:? "UpdateDate")
            <*> (x .:? "CreateDate")
            <*> (x .:? "Targets" .!= mempty)
            <*> (x .:? "ErrorCode")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable Operation

instance NFData Operation
