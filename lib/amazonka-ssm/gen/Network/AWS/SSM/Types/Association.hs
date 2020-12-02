{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Association
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Association where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.Target

-- | Describes an association of a Systems Manager document and an instance.
--
--
--
-- /See:/ 'association' smart constructor.
data Association = Association'
  { _assAssociationId :: !(Maybe Text),
    _assInstanceId :: !(Maybe Text),
    _assOverview :: !(Maybe AssociationOverview),
    _assLastExecutionDate :: !(Maybe POSIX),
    _assScheduleExpression :: !(Maybe Text),
    _assName :: !(Maybe Text),
    _assTargets :: !(Maybe [Target]),
    _assDocumentVersion :: !(Maybe Text),
    _assAssociationVersion :: !(Maybe Text),
    _assAssociationName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Association' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'assAssociationId' - The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
--
-- * 'assInstanceId' - The ID of the instance.
--
-- * 'assOverview' - Information about the association.
--
-- * 'assLastExecutionDate' - The date on which the association was last run.
--
-- * 'assScheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- * 'assName' - The name of the Systems Manager document.
--
-- * 'assTargets' - The instances targeted by the request to create an association.
--
-- * 'assDocumentVersion' - The version of the document used in the association.
--
-- * 'assAssociationVersion' - The association version.
--
-- * 'assAssociationName' - The association name.
association ::
  Association
association =
  Association'
    { _assAssociationId = Nothing,
      _assInstanceId = Nothing,
      _assOverview = Nothing,
      _assLastExecutionDate = Nothing,
      _assScheduleExpression = Nothing,
      _assName = Nothing,
      _assTargets = Nothing,
      _assDocumentVersion = Nothing,
      _assAssociationVersion = Nothing,
      _assAssociationName = Nothing
    }

-- | The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
assAssociationId :: Lens' Association (Maybe Text)
assAssociationId = lens _assAssociationId (\s a -> s {_assAssociationId = a})

-- | The ID of the instance.
assInstanceId :: Lens' Association (Maybe Text)
assInstanceId = lens _assInstanceId (\s a -> s {_assInstanceId = a})

-- | Information about the association.
assOverview :: Lens' Association (Maybe AssociationOverview)
assOverview = lens _assOverview (\s a -> s {_assOverview = a})

-- | The date on which the association was last run.
assLastExecutionDate :: Lens' Association (Maybe UTCTime)
assLastExecutionDate = lens _assLastExecutionDate (\s a -> s {_assLastExecutionDate = a}) . mapping _Time

-- | A cron expression that specifies a schedule when the association runs.
assScheduleExpression :: Lens' Association (Maybe Text)
assScheduleExpression = lens _assScheduleExpression (\s a -> s {_assScheduleExpression = a})

-- | The name of the Systems Manager document.
assName :: Lens' Association (Maybe Text)
assName = lens _assName (\s a -> s {_assName = a})

-- | The instances targeted by the request to create an association.
assTargets :: Lens' Association [Target]
assTargets = lens _assTargets (\s a -> s {_assTargets = a}) . _Default . _Coerce

-- | The version of the document used in the association.
assDocumentVersion :: Lens' Association (Maybe Text)
assDocumentVersion = lens _assDocumentVersion (\s a -> s {_assDocumentVersion = a})

-- | The association version.
assAssociationVersion :: Lens' Association (Maybe Text)
assAssociationVersion = lens _assAssociationVersion (\s a -> s {_assAssociationVersion = a})

-- | The association name.
assAssociationName :: Lens' Association (Maybe Text)
assAssociationName = lens _assAssociationName (\s a -> s {_assAssociationName = a})

instance FromJSON Association where
  parseJSON =
    withObject
      "Association"
      ( \x ->
          Association'
            <$> (x .:? "AssociationId")
            <*> (x .:? "InstanceId")
            <*> (x .:? "Overview")
            <*> (x .:? "LastExecutionDate")
            <*> (x .:? "ScheduleExpression")
            <*> (x .:? "Name")
            <*> (x .:? "Targets" .!= mempty)
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "AssociationVersion")
            <*> (x .:? "AssociationName")
      )

instance Hashable Association

instance NFData Association
