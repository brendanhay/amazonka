{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionTarget where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.OutputSource

-- | Includes information about the specified association execution.
--
--
--
-- /See:/ 'associationExecutionTarget' smart constructor.
data AssociationExecutionTarget = AssociationExecutionTarget'
  { _aetAssociationId ::
      !(Maybe Text),
    _aetDetailedStatus :: !(Maybe Text),
    _aetStatus :: !(Maybe Text),
    _aetExecutionId :: !(Maybe Text),
    _aetResourceId :: !(Maybe Text),
    _aetResourceType :: !(Maybe Text),
    _aetOutputSource ::
      !(Maybe OutputSource),
    _aetLastExecutionDate ::
      !(Maybe POSIX),
    _aetAssociationVersion ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociationExecutionTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aetAssociationId' - The association ID.
--
-- * 'aetDetailedStatus' - Detailed information about the execution status.
--
-- * 'aetStatus' - The association execution status.
--
-- * 'aetExecutionId' - The execution ID.
--
-- * 'aetResourceId' - The resource ID, for example, the instance ID where the association ran.
--
-- * 'aetResourceType' - The resource type, for example, instance.
--
-- * 'aetOutputSource' - The location where the association details are saved.
--
-- * 'aetLastExecutionDate' - The date of the last execution.
--
-- * 'aetAssociationVersion' - The association version.
associationExecutionTarget ::
  AssociationExecutionTarget
associationExecutionTarget =
  AssociationExecutionTarget'
    { _aetAssociationId = Nothing,
      _aetDetailedStatus = Nothing,
      _aetStatus = Nothing,
      _aetExecutionId = Nothing,
      _aetResourceId = Nothing,
      _aetResourceType = Nothing,
      _aetOutputSource = Nothing,
      _aetLastExecutionDate = Nothing,
      _aetAssociationVersion = Nothing
    }

-- | The association ID.
aetAssociationId :: Lens' AssociationExecutionTarget (Maybe Text)
aetAssociationId = lens _aetAssociationId (\s a -> s {_aetAssociationId = a})

-- | Detailed information about the execution status.
aetDetailedStatus :: Lens' AssociationExecutionTarget (Maybe Text)
aetDetailedStatus = lens _aetDetailedStatus (\s a -> s {_aetDetailedStatus = a})

-- | The association execution status.
aetStatus :: Lens' AssociationExecutionTarget (Maybe Text)
aetStatus = lens _aetStatus (\s a -> s {_aetStatus = a})

-- | The execution ID.
aetExecutionId :: Lens' AssociationExecutionTarget (Maybe Text)
aetExecutionId = lens _aetExecutionId (\s a -> s {_aetExecutionId = a})

-- | The resource ID, for example, the instance ID where the association ran.
aetResourceId :: Lens' AssociationExecutionTarget (Maybe Text)
aetResourceId = lens _aetResourceId (\s a -> s {_aetResourceId = a})

-- | The resource type, for example, instance.
aetResourceType :: Lens' AssociationExecutionTarget (Maybe Text)
aetResourceType = lens _aetResourceType (\s a -> s {_aetResourceType = a})

-- | The location where the association details are saved.
aetOutputSource :: Lens' AssociationExecutionTarget (Maybe OutputSource)
aetOutputSource = lens _aetOutputSource (\s a -> s {_aetOutputSource = a})

-- | The date of the last execution.
aetLastExecutionDate :: Lens' AssociationExecutionTarget (Maybe UTCTime)
aetLastExecutionDate = lens _aetLastExecutionDate (\s a -> s {_aetLastExecutionDate = a}) . mapping _Time

-- | The association version.
aetAssociationVersion :: Lens' AssociationExecutionTarget (Maybe Text)
aetAssociationVersion = lens _aetAssociationVersion (\s a -> s {_aetAssociationVersion = a})

instance FromJSON AssociationExecutionTarget where
  parseJSON =
    withObject
      "AssociationExecutionTarget"
      ( \x ->
          AssociationExecutionTarget'
            <$> (x .:? "AssociationId")
            <*> (x .:? "DetailedStatus")
            <*> (x .:? "Status")
            <*> (x .:? "ExecutionId")
            <*> (x .:? "ResourceId")
            <*> (x .:? "ResourceType")
            <*> (x .:? "OutputSource")
            <*> (x .:? "LastExecutionDate")
            <*> (x .:? "AssociationVersion")
      )

instance Hashable AssociationExecutionTarget

instance NFData AssociationExecutionTarget
