{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Finding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Finding where

import Network.AWS.GuardDuty.Types.Resource
import Network.AWS.GuardDuty.Types.ServiceInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the finding, which is generated when abnormal or suspicious activity is detected.
--
--
--
-- /See:/ 'finding' smart constructor.
data Finding = Finding'
  { _fService :: !(Maybe ServiceInfo),
    _fConfidence :: !(Maybe Double),
    _fPartition :: !(Maybe Text),
    _fTitle :: !(Maybe Text),
    _fDescription :: !(Maybe Text),
    _fAccountId :: !Text,
    _fARN :: !Text,
    _fCreatedAt :: !Text,
    _fId :: !Text,
    _fRegion :: !Text,
    _fResource :: !Resource,
    _fSchemaVersion :: !Text,
    _fSeverity :: !Double,
    _fType :: !Text,
    _fUpdatedAt :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Finding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fService' - Undocumented member.
--
-- * 'fConfidence' - The confidence score for the finding.
--
-- * 'fPartition' - The partition associated with the finding.
--
-- * 'fTitle' - The title of the finding.
--
-- * 'fDescription' - The description of the finding.
--
-- * 'fAccountId' - The ID of the account in which the finding was generated.
--
-- * 'fARN' - The ARN of the finding.
--
-- * 'fCreatedAt' - The time and date when the finding was created.
--
-- * 'fId' - The ID of the finding.
--
-- * 'fRegion' - The Region where the finding was generated.
--
-- * 'fResource' - Undocumented member.
--
-- * 'fSchemaVersion' - The version of the schema used for the finding.
--
-- * 'fSeverity' - The severity of the finding.
--
-- * 'fType' - The type of finding.
--
-- * 'fUpdatedAt' - The time and date when the finding was last updated.
finding ::
  -- | 'fAccountId'
  Text ->
  -- | 'fARN'
  Text ->
  -- | 'fCreatedAt'
  Text ->
  -- | 'fId'
  Text ->
  -- | 'fRegion'
  Text ->
  -- | 'fResource'
  Resource ->
  -- | 'fSchemaVersion'
  Text ->
  -- | 'fSeverity'
  Double ->
  -- | 'fType'
  Text ->
  -- | 'fUpdatedAt'
  Text ->
  Finding
finding
  pAccountId_
  pARN_
  pCreatedAt_
  pId_
  pRegion_
  pResource_
  pSchemaVersion_
  pSeverity_
  pType_
  pUpdatedAt_ =
    Finding'
      { _fService = Nothing,
        _fConfidence = Nothing,
        _fPartition = Nothing,
        _fTitle = Nothing,
        _fDescription = Nothing,
        _fAccountId = pAccountId_,
        _fARN = pARN_,
        _fCreatedAt = pCreatedAt_,
        _fId = pId_,
        _fRegion = pRegion_,
        _fResource = pResource_,
        _fSchemaVersion = pSchemaVersion_,
        _fSeverity = pSeverity_,
        _fType = pType_,
        _fUpdatedAt = pUpdatedAt_
      }

-- | Undocumented member.
fService :: Lens' Finding (Maybe ServiceInfo)
fService = lens _fService (\s a -> s {_fService = a})

-- | The confidence score for the finding.
fConfidence :: Lens' Finding (Maybe Double)
fConfidence = lens _fConfidence (\s a -> s {_fConfidence = a})

-- | The partition associated with the finding.
fPartition :: Lens' Finding (Maybe Text)
fPartition = lens _fPartition (\s a -> s {_fPartition = a})

-- | The title of the finding.
fTitle :: Lens' Finding (Maybe Text)
fTitle = lens _fTitle (\s a -> s {_fTitle = a})

-- | The description of the finding.
fDescription :: Lens' Finding (Maybe Text)
fDescription = lens _fDescription (\s a -> s {_fDescription = a})

-- | The ID of the account in which the finding was generated.
fAccountId :: Lens' Finding Text
fAccountId = lens _fAccountId (\s a -> s {_fAccountId = a})

-- | The ARN of the finding.
fARN :: Lens' Finding Text
fARN = lens _fARN (\s a -> s {_fARN = a})

-- | The time and date when the finding was created.
fCreatedAt :: Lens' Finding Text
fCreatedAt = lens _fCreatedAt (\s a -> s {_fCreatedAt = a})

-- | The ID of the finding.
fId :: Lens' Finding Text
fId = lens _fId (\s a -> s {_fId = a})

-- | The Region where the finding was generated.
fRegion :: Lens' Finding Text
fRegion = lens _fRegion (\s a -> s {_fRegion = a})

-- | Undocumented member.
fResource :: Lens' Finding Resource
fResource = lens _fResource (\s a -> s {_fResource = a})

-- | The version of the schema used for the finding.
fSchemaVersion :: Lens' Finding Text
fSchemaVersion = lens _fSchemaVersion (\s a -> s {_fSchemaVersion = a})

-- | The severity of the finding.
fSeverity :: Lens' Finding Double
fSeverity = lens _fSeverity (\s a -> s {_fSeverity = a})

-- | The type of finding.
fType :: Lens' Finding Text
fType = lens _fType (\s a -> s {_fType = a})

-- | The time and date when the finding was last updated.
fUpdatedAt :: Lens' Finding Text
fUpdatedAt = lens _fUpdatedAt (\s a -> s {_fUpdatedAt = a})

instance FromJSON Finding where
  parseJSON =
    withObject
      "Finding"
      ( \x ->
          Finding'
            <$> (x .:? "service")
            <*> (x .:? "confidence")
            <*> (x .:? "partition")
            <*> (x .:? "title")
            <*> (x .:? "description")
            <*> (x .: "accountId")
            <*> (x .: "arn")
            <*> (x .: "createdAt")
            <*> (x .: "id")
            <*> (x .: "region")
            <*> (x .: "resource")
            <*> (x .: "schemaVersion")
            <*> (x .: "severity")
            <*> (x .: "type")
            <*> (x .: "updatedAt")
      )

instance Hashable Finding

instance NFData Finding
