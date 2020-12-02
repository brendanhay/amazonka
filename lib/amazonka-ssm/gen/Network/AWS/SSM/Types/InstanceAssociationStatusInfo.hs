{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociationStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationStatusInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.InstanceAssociationOutputURL

-- | Status information about the instance association.
--
--
--
-- /See:/ 'instanceAssociationStatusInfo' smart constructor.
data InstanceAssociationStatusInfo = InstanceAssociationStatusInfo'
  { _iasiAssociationId ::
      !(Maybe Text),
    _iasiInstanceId ::
      !(Maybe Text),
    _iasiDetailedStatus ::
      !(Maybe Text),
    _iasiStatus :: !(Maybe Text),
    _iasiOutputURL ::
      !( Maybe
           InstanceAssociationOutputURL
       ),
    _iasiExecutionSummary ::
      !(Maybe Text),
    _iasiName :: !(Maybe Text),
    _iasiErrorCode :: !(Maybe Text),
    _iasiDocumentVersion ::
      !(Maybe Text),
    _iasiAssociationVersion ::
      !(Maybe Text),
    _iasiExecutionDate ::
      !(Maybe POSIX),
    _iasiAssociationName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceAssociationStatusInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iasiAssociationId' - The association ID.
--
-- * 'iasiInstanceId' - The instance ID where the association was created.
--
-- * 'iasiDetailedStatus' - Detailed status information about the instance association.
--
-- * 'iasiStatus' - Status information about the instance association.
--
-- * 'iasiOutputURL' - A URL for an S3 bucket where you want to store the results of this request.
--
-- * 'iasiExecutionSummary' - Summary information about association execution.
--
-- * 'iasiName' - The name of the association.
--
-- * 'iasiErrorCode' - An error code returned by the request to create the association.
--
-- * 'iasiDocumentVersion' - The association document versions.
--
-- * 'iasiAssociationVersion' - The version of the association applied to the instance.
--
-- * 'iasiExecutionDate' - The date the instance association ran.
--
-- * 'iasiAssociationName' - The name of the association applied to the instance.
instanceAssociationStatusInfo ::
  InstanceAssociationStatusInfo
instanceAssociationStatusInfo =
  InstanceAssociationStatusInfo'
    { _iasiAssociationId = Nothing,
      _iasiInstanceId = Nothing,
      _iasiDetailedStatus = Nothing,
      _iasiStatus = Nothing,
      _iasiOutputURL = Nothing,
      _iasiExecutionSummary = Nothing,
      _iasiName = Nothing,
      _iasiErrorCode = Nothing,
      _iasiDocumentVersion = Nothing,
      _iasiAssociationVersion = Nothing,
      _iasiExecutionDate = Nothing,
      _iasiAssociationName = Nothing
    }

-- | The association ID.
iasiAssociationId :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiAssociationId = lens _iasiAssociationId (\s a -> s {_iasiAssociationId = a})

-- | The instance ID where the association was created.
iasiInstanceId :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiInstanceId = lens _iasiInstanceId (\s a -> s {_iasiInstanceId = a})

-- | Detailed status information about the instance association.
iasiDetailedStatus :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiDetailedStatus = lens _iasiDetailedStatus (\s a -> s {_iasiDetailedStatus = a})

-- | Status information about the instance association.
iasiStatus :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiStatus = lens _iasiStatus (\s a -> s {_iasiStatus = a})

-- | A URL for an S3 bucket where you want to store the results of this request.
iasiOutputURL :: Lens' InstanceAssociationStatusInfo (Maybe InstanceAssociationOutputURL)
iasiOutputURL = lens _iasiOutputURL (\s a -> s {_iasiOutputURL = a})

-- | Summary information about association execution.
iasiExecutionSummary :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiExecutionSummary = lens _iasiExecutionSummary (\s a -> s {_iasiExecutionSummary = a})

-- | The name of the association.
iasiName :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiName = lens _iasiName (\s a -> s {_iasiName = a})

-- | An error code returned by the request to create the association.
iasiErrorCode :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiErrorCode = lens _iasiErrorCode (\s a -> s {_iasiErrorCode = a})

-- | The association document versions.
iasiDocumentVersion :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiDocumentVersion = lens _iasiDocumentVersion (\s a -> s {_iasiDocumentVersion = a})

-- | The version of the association applied to the instance.
iasiAssociationVersion :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiAssociationVersion = lens _iasiAssociationVersion (\s a -> s {_iasiAssociationVersion = a})

-- | The date the instance association ran.
iasiExecutionDate :: Lens' InstanceAssociationStatusInfo (Maybe UTCTime)
iasiExecutionDate = lens _iasiExecutionDate (\s a -> s {_iasiExecutionDate = a}) . mapping _Time

-- | The name of the association applied to the instance.
iasiAssociationName :: Lens' InstanceAssociationStatusInfo (Maybe Text)
iasiAssociationName = lens _iasiAssociationName (\s a -> s {_iasiAssociationName = a})

instance FromJSON InstanceAssociationStatusInfo where
  parseJSON =
    withObject
      "InstanceAssociationStatusInfo"
      ( \x ->
          InstanceAssociationStatusInfo'
            <$> (x .:? "AssociationId")
            <*> (x .:? "InstanceId")
            <*> (x .:? "DetailedStatus")
            <*> (x .:? "Status")
            <*> (x .:? "OutputUrl")
            <*> (x .:? "ExecutionSummary")
            <*> (x .:? "Name")
            <*> (x .:? "ErrorCode")
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "AssociationVersion")
            <*> (x .:? "ExecutionDate")
            <*> (x .:? "AssociationName")
      )

instance Hashable InstanceAssociationStatusInfo

instance NFData InstanceAssociationStatusInfo
