{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecution where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Includes information about the specified association.
--
--
--
-- /See:/ 'associationExecution' smart constructor.
data AssociationExecution = AssociationExecution'
  { _aeAssociationId ::
      !(Maybe Text),
    _aeDetailedStatus :: !(Maybe Text),
    _aeStatus :: !(Maybe Text),
    _aeExecutionId :: !(Maybe Text),
    _aeCreatedTime :: !(Maybe POSIX),
    _aeResourceCountByStatus :: !(Maybe Text),
    _aeLastExecutionDate :: !(Maybe POSIX),
    _aeAssociationVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociationExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeAssociationId' - The association ID.
--
-- * 'aeDetailedStatus' - Detailed status information about the execution.
--
-- * 'aeStatus' - The status of the association execution.
--
-- * 'aeExecutionId' - The execution ID for the association.
--
-- * 'aeCreatedTime' - The time the execution started.
--
-- * 'aeResourceCountByStatus' - An aggregate status of the resources in the execution based on the status type.
--
-- * 'aeLastExecutionDate' - The date of the last execution.
--
-- * 'aeAssociationVersion' - The association version.
associationExecution ::
  AssociationExecution
associationExecution =
  AssociationExecution'
    { _aeAssociationId = Nothing,
      _aeDetailedStatus = Nothing,
      _aeStatus = Nothing,
      _aeExecutionId = Nothing,
      _aeCreatedTime = Nothing,
      _aeResourceCountByStatus = Nothing,
      _aeLastExecutionDate = Nothing,
      _aeAssociationVersion = Nothing
    }

-- | The association ID.
aeAssociationId :: Lens' AssociationExecution (Maybe Text)
aeAssociationId = lens _aeAssociationId (\s a -> s {_aeAssociationId = a})

-- | Detailed status information about the execution.
aeDetailedStatus :: Lens' AssociationExecution (Maybe Text)
aeDetailedStatus = lens _aeDetailedStatus (\s a -> s {_aeDetailedStatus = a})

-- | The status of the association execution.
aeStatus :: Lens' AssociationExecution (Maybe Text)
aeStatus = lens _aeStatus (\s a -> s {_aeStatus = a})

-- | The execution ID for the association.
aeExecutionId :: Lens' AssociationExecution (Maybe Text)
aeExecutionId = lens _aeExecutionId (\s a -> s {_aeExecutionId = a})

-- | The time the execution started.
aeCreatedTime :: Lens' AssociationExecution (Maybe UTCTime)
aeCreatedTime = lens _aeCreatedTime (\s a -> s {_aeCreatedTime = a}) . mapping _Time

-- | An aggregate status of the resources in the execution based on the status type.
aeResourceCountByStatus :: Lens' AssociationExecution (Maybe Text)
aeResourceCountByStatus = lens _aeResourceCountByStatus (\s a -> s {_aeResourceCountByStatus = a})

-- | The date of the last execution.
aeLastExecutionDate :: Lens' AssociationExecution (Maybe UTCTime)
aeLastExecutionDate = lens _aeLastExecutionDate (\s a -> s {_aeLastExecutionDate = a}) . mapping _Time

-- | The association version.
aeAssociationVersion :: Lens' AssociationExecution (Maybe Text)
aeAssociationVersion = lens _aeAssociationVersion (\s a -> s {_aeAssociationVersion = a})

instance FromJSON AssociationExecution where
  parseJSON =
    withObject
      "AssociationExecution"
      ( \x ->
          AssociationExecution'
            <$> (x .:? "AssociationId")
            <*> (x .:? "DetailedStatus")
            <*> (x .:? "Status")
            <*> (x .:? "ExecutionId")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "ResourceCountByStatus")
            <*> (x .:? "LastExecutionDate")
            <*> (x .:? "AssociationVersion")
      )

instance Hashable AssociationExecution

instance NFData AssociationExecution
