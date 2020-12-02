{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Finding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Finding where

import Network.AWS.Inspector.Types.AssetAttributes
import Network.AWS.Inspector.Types.AssetType
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.InspectorServiceAttributes
import Network.AWS.Inspector.Types.Severity
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an Amazon Inspector finding. This data type is used as the response element in the 'DescribeFindings' action.
--
--
--
-- /See:/ 'finding' smart constructor.
data Finding = Finding'
  { _fService :: !(Maybe Text),
    _fSeverity :: !(Maybe Severity),
    _fSchemaVersion :: !(Maybe Nat),
    _fConfidence :: !(Maybe Nat),
    _fAssetAttributes :: !(Maybe AssetAttributes),
    _fServiceAttributes :: !(Maybe InspectorServiceAttributes),
    _fId :: !(Maybe Text),
    _fNumericSeverity :: !(Maybe Double),
    _fAssetType :: !(Maybe AssetType),
    _fTitle :: !(Maybe Text),
    _fIndicatorOfCompromise :: !(Maybe Bool),
    _fDescription :: !(Maybe Text),
    _fRecommendation :: !(Maybe Text),
    _fArn :: !Text,
    _fAttributes :: ![Attribute],
    _fUserAttributes :: ![Attribute],
    _fCreatedAt :: !POSIX,
    _fUpdatedAt :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Finding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fService' - The data element is set to "Inspector".
--
-- * 'fSeverity' - The finding severity. Values can be set to High, Medium, Low, and Informational.
--
-- * 'fSchemaVersion' - The schema version of this data type.
--
-- * 'fConfidence' - This data element is currently not used.
--
-- * 'fAssetAttributes' - A collection of attributes of the host from which the finding is generated.
--
-- * 'fServiceAttributes' - This data type is used in the 'Finding' data type.
--
-- * 'fId' - The ID of the finding.
--
-- * 'fNumericSeverity' - The numeric value of the finding severity.
--
-- * 'fAssetType' - The type of the host from which the finding is generated.
--
-- * 'fTitle' - The name of the finding.
--
-- * 'fIndicatorOfCompromise' - This data element is currently not used.
--
-- * 'fDescription' - The description of the finding.
--
-- * 'fRecommendation' - The recommendation for the finding.
--
-- * 'fArn' - The ARN that specifies the finding.
--
-- * 'fAttributes' - The system-defined attributes for the finding.
--
-- * 'fUserAttributes' - The user-defined attributes that are assigned to the finding.
--
-- * 'fCreatedAt' - The time when the finding was generated.
--
-- * 'fUpdatedAt' - The time when 'AddAttributesToFindings' is called.
finding ::
  -- | 'fArn'
  Text ->
  -- | 'fCreatedAt'
  UTCTime ->
  -- | 'fUpdatedAt'
  UTCTime ->
  Finding
finding pArn_ pCreatedAt_ pUpdatedAt_ =
  Finding'
    { _fService = Nothing,
      _fSeverity = Nothing,
      _fSchemaVersion = Nothing,
      _fConfidence = Nothing,
      _fAssetAttributes = Nothing,
      _fServiceAttributes = Nothing,
      _fId = Nothing,
      _fNumericSeverity = Nothing,
      _fAssetType = Nothing,
      _fTitle = Nothing,
      _fIndicatorOfCompromise = Nothing,
      _fDescription = Nothing,
      _fRecommendation = Nothing,
      _fArn = pArn_,
      _fAttributes = mempty,
      _fUserAttributes = mempty,
      _fCreatedAt = _Time # pCreatedAt_,
      _fUpdatedAt = _Time # pUpdatedAt_
    }

-- | The data element is set to "Inspector".
fService :: Lens' Finding (Maybe Text)
fService = lens _fService (\s a -> s {_fService = a})

-- | The finding severity. Values can be set to High, Medium, Low, and Informational.
fSeverity :: Lens' Finding (Maybe Severity)
fSeverity = lens _fSeverity (\s a -> s {_fSeverity = a})

-- | The schema version of this data type.
fSchemaVersion :: Lens' Finding (Maybe Natural)
fSchemaVersion = lens _fSchemaVersion (\s a -> s {_fSchemaVersion = a}) . mapping _Nat

-- | This data element is currently not used.
fConfidence :: Lens' Finding (Maybe Natural)
fConfidence = lens _fConfidence (\s a -> s {_fConfidence = a}) . mapping _Nat

-- | A collection of attributes of the host from which the finding is generated.
fAssetAttributes :: Lens' Finding (Maybe AssetAttributes)
fAssetAttributes = lens _fAssetAttributes (\s a -> s {_fAssetAttributes = a})

-- | This data type is used in the 'Finding' data type.
fServiceAttributes :: Lens' Finding (Maybe InspectorServiceAttributes)
fServiceAttributes = lens _fServiceAttributes (\s a -> s {_fServiceAttributes = a})

-- | The ID of the finding.
fId :: Lens' Finding (Maybe Text)
fId = lens _fId (\s a -> s {_fId = a})

-- | The numeric value of the finding severity.
fNumericSeverity :: Lens' Finding (Maybe Double)
fNumericSeverity = lens _fNumericSeverity (\s a -> s {_fNumericSeverity = a})

-- | The type of the host from which the finding is generated.
fAssetType :: Lens' Finding (Maybe AssetType)
fAssetType = lens _fAssetType (\s a -> s {_fAssetType = a})

-- | The name of the finding.
fTitle :: Lens' Finding (Maybe Text)
fTitle = lens _fTitle (\s a -> s {_fTitle = a})

-- | This data element is currently not used.
fIndicatorOfCompromise :: Lens' Finding (Maybe Bool)
fIndicatorOfCompromise = lens _fIndicatorOfCompromise (\s a -> s {_fIndicatorOfCompromise = a})

-- | The description of the finding.
fDescription :: Lens' Finding (Maybe Text)
fDescription = lens _fDescription (\s a -> s {_fDescription = a})

-- | The recommendation for the finding.
fRecommendation :: Lens' Finding (Maybe Text)
fRecommendation = lens _fRecommendation (\s a -> s {_fRecommendation = a})

-- | The ARN that specifies the finding.
fArn :: Lens' Finding Text
fArn = lens _fArn (\s a -> s {_fArn = a})

-- | The system-defined attributes for the finding.
fAttributes :: Lens' Finding [Attribute]
fAttributes = lens _fAttributes (\s a -> s {_fAttributes = a}) . _Coerce

-- | The user-defined attributes that are assigned to the finding.
fUserAttributes :: Lens' Finding [Attribute]
fUserAttributes = lens _fUserAttributes (\s a -> s {_fUserAttributes = a}) . _Coerce

-- | The time when the finding was generated.
fCreatedAt :: Lens' Finding UTCTime
fCreatedAt = lens _fCreatedAt (\s a -> s {_fCreatedAt = a}) . _Time

-- | The time when 'AddAttributesToFindings' is called.
fUpdatedAt :: Lens' Finding UTCTime
fUpdatedAt = lens _fUpdatedAt (\s a -> s {_fUpdatedAt = a}) . _Time

instance FromJSON Finding where
  parseJSON =
    withObject
      "Finding"
      ( \x ->
          Finding'
            <$> (x .:? "service")
            <*> (x .:? "severity")
            <*> (x .:? "schemaVersion")
            <*> (x .:? "confidence")
            <*> (x .:? "assetAttributes")
            <*> (x .:? "serviceAttributes")
            <*> (x .:? "id")
            <*> (x .:? "numericSeverity")
            <*> (x .:? "assetType")
            <*> (x .:? "title")
            <*> (x .:? "indicatorOfCompromise")
            <*> (x .:? "description")
            <*> (x .:? "recommendation")
            <*> (x .: "arn")
            <*> (x .:? "attributes" .!= mempty)
            <*> (x .:? "userAttributes" .!= mempty)
            <*> (x .: "createdAt")
            <*> (x .: "updatedAt")
      )

instance Hashable Finding

instance NFData Finding
