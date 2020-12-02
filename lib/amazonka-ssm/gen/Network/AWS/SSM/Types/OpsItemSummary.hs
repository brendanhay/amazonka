{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.OpsItemDataValue
import Network.AWS.SSM.Types.OpsItemStatus

-- | A count of OpsItems.
--
--
--
-- /See:/ 'opsItemSummary' smart constructor.
data OpsItemSummary = OpsItemSummary'
  { _oisOpsItemId ::
      !(Maybe Text),
    _oisStatus :: !(Maybe OpsItemStatus),
    _oisPriority :: !(Maybe Nat),
    _oisCreatedTime :: !(Maybe POSIX),
    _oisCategory :: !(Maybe Text),
    _oisSeverity :: !(Maybe Text),
    _oisCreatedBy :: !(Maybe Text),
    _oisLastModifiedTime :: !(Maybe POSIX),
    _oisSource :: !(Maybe Text),
    _oisTitle :: !(Maybe Text),
    _oisLastModifiedBy :: !(Maybe Text),
    _oisOperationalData :: !(Maybe (Map Text (OpsItemDataValue)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OpsItemSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oisOpsItemId' - The ID of the OpsItem.
--
-- * 'oisStatus' - The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ .
--
-- * 'oisPriority' - The importance of this OpsItem in relation to other OpsItems in the system.
--
-- * 'oisCreatedTime' - The date and time the OpsItem was created.
--
-- * 'oisCategory' - A list of OpsItems by category.
--
-- * 'oisSeverity' - A list of OpsItems by severity.
--
-- * 'oisCreatedBy' - The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
--
-- * 'oisLastModifiedTime' - The date and time the OpsItem was last updated.
--
-- * 'oisSource' - The impacted AWS resource.
--
-- * 'oisTitle' - A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- * 'oisLastModifiedBy' - The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
--
-- * 'oisOperationalData' - Operational data is custom data that provides useful reference details about the OpsItem.
opsItemSummary ::
  OpsItemSummary
opsItemSummary =
  OpsItemSummary'
    { _oisOpsItemId = Nothing,
      _oisStatus = Nothing,
      _oisPriority = Nothing,
      _oisCreatedTime = Nothing,
      _oisCategory = Nothing,
      _oisSeverity = Nothing,
      _oisCreatedBy = Nothing,
      _oisLastModifiedTime = Nothing,
      _oisSource = Nothing,
      _oisTitle = Nothing,
      _oisLastModifiedBy = Nothing,
      _oisOperationalData = Nothing
    }

-- | The ID of the OpsItem.
oisOpsItemId :: Lens' OpsItemSummary (Maybe Text)
oisOpsItemId = lens _oisOpsItemId (\s a -> s {_oisOpsItemId = a})

-- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ .
oisStatus :: Lens' OpsItemSummary (Maybe OpsItemStatus)
oisStatus = lens _oisStatus (\s a -> s {_oisStatus = a})

-- | The importance of this OpsItem in relation to other OpsItems in the system.
oisPriority :: Lens' OpsItemSummary (Maybe Natural)
oisPriority = lens _oisPriority (\s a -> s {_oisPriority = a}) . mapping _Nat

-- | The date and time the OpsItem was created.
oisCreatedTime :: Lens' OpsItemSummary (Maybe UTCTime)
oisCreatedTime = lens _oisCreatedTime (\s a -> s {_oisCreatedTime = a}) . mapping _Time

-- | A list of OpsItems by category.
oisCategory :: Lens' OpsItemSummary (Maybe Text)
oisCategory = lens _oisCategory (\s a -> s {_oisCategory = a})

-- | A list of OpsItems by severity.
oisSeverity :: Lens' OpsItemSummary (Maybe Text)
oisSeverity = lens _oisSeverity (\s a -> s {_oisSeverity = a})

-- | The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
oisCreatedBy :: Lens' OpsItemSummary (Maybe Text)
oisCreatedBy = lens _oisCreatedBy (\s a -> s {_oisCreatedBy = a})

-- | The date and time the OpsItem was last updated.
oisLastModifiedTime :: Lens' OpsItemSummary (Maybe UTCTime)
oisLastModifiedTime = lens _oisLastModifiedTime (\s a -> s {_oisLastModifiedTime = a}) . mapping _Time

-- | The impacted AWS resource.
oisSource :: Lens' OpsItemSummary (Maybe Text)
oisSource = lens _oisSource (\s a -> s {_oisSource = a})

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
oisTitle :: Lens' OpsItemSummary (Maybe Text)
oisTitle = lens _oisTitle (\s a -> s {_oisTitle = a})

-- | The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
oisLastModifiedBy :: Lens' OpsItemSummary (Maybe Text)
oisLastModifiedBy = lens _oisLastModifiedBy (\s a -> s {_oisLastModifiedBy = a})

-- | Operational data is custom data that provides useful reference details about the OpsItem.
oisOperationalData :: Lens' OpsItemSummary (HashMap Text (OpsItemDataValue))
oisOperationalData = lens _oisOperationalData (\s a -> s {_oisOperationalData = a}) . _Default . _Map

instance FromJSON OpsItemSummary where
  parseJSON =
    withObject
      "OpsItemSummary"
      ( \x ->
          OpsItemSummary'
            <$> (x .:? "OpsItemId")
            <*> (x .:? "Status")
            <*> (x .:? "Priority")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "Category")
            <*> (x .:? "Severity")
            <*> (x .:? "CreatedBy")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "Source")
            <*> (x .:? "Title")
            <*> (x .:? "LastModifiedBy")
            <*> (x .:? "OperationalData" .!= mempty)
      )

instance Hashable OpsItemSummary

instance NFData OpsItemSummary
