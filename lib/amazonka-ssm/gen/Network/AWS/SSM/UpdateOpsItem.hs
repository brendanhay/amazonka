{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateOpsItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Edit or change an OpsItem. You must have permission in AWS Identity and Access Management (IAM) to update an OpsItem. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
--
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.UpdateOpsItem
  ( -- * Creating a Request
    updateOpsItem,
    UpdateOpsItem,

    -- * Request Lenses
    uoiStatus,
    uoiOperationalDataToDelete,
    uoiPriority,
    uoiCategory,
    uoiSeverity,
    uoiRelatedOpsItems,
    uoiTitle,
    uoiOperationalData,
    uoiDescription,
    uoiNotifications,
    uoiOpsItemId,

    -- * Destructuring the Response
    updateOpsItemResponse,
    UpdateOpsItemResponse,

    -- * Response Lenses
    uoirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'updateOpsItem' smart constructor.
data UpdateOpsItem = UpdateOpsItem'
  { _uoiStatus ::
      !(Maybe OpsItemStatus),
    _uoiOperationalDataToDelete :: !(Maybe [Text]),
    _uoiPriority :: !(Maybe Nat),
    _uoiCategory :: !(Maybe Text),
    _uoiSeverity :: !(Maybe Text),
    _uoiRelatedOpsItems :: !(Maybe [RelatedOpsItem]),
    _uoiTitle :: !(Maybe Text),
    _uoiOperationalData :: !(Maybe (Map Text (OpsItemDataValue))),
    _uoiDescription :: !(Maybe Text),
    _uoiNotifications :: !(Maybe [OpsItemNotification]),
    _uoiOpsItemId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateOpsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uoiStatus' - The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
--
-- * 'uoiOperationalDataToDelete' - Keys that you want to remove from the OperationalData map.
--
-- * 'uoiPriority' - The importance of this OpsItem in relation to other OpsItems in the system.
--
-- * 'uoiCategory' - Specify a new category for an OpsItem.
--
-- * 'uoiSeverity' - Specify a new severity for an OpsItem.
--
-- * 'uoiRelatedOpsItems' - One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- * 'uoiTitle' - A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- * 'uoiOperationalData' - Add new keys or edit existing key-value pairs of the OperationalData map in the OpsItem object. Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB. /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm. You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action). Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
--
-- * 'uoiDescription' - Update the information about the OpsItem. Provide enough information so that users reading this OpsItem for the first time understand the issue.
--
-- * 'uoiNotifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- * 'uoiOpsItemId' - The ID of the OpsItem.
updateOpsItem ::
  -- | 'uoiOpsItemId'
  Text ->
  UpdateOpsItem
updateOpsItem pOpsItemId_ =
  UpdateOpsItem'
    { _uoiStatus = Nothing,
      _uoiOperationalDataToDelete = Nothing,
      _uoiPriority = Nothing,
      _uoiCategory = Nothing,
      _uoiSeverity = Nothing,
      _uoiRelatedOpsItems = Nothing,
      _uoiTitle = Nothing,
      _uoiOperationalData = Nothing,
      _uoiDescription = Nothing,
      _uoiNotifications = Nothing,
      _uoiOpsItemId = pOpsItemId_
    }

-- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
uoiStatus :: Lens' UpdateOpsItem (Maybe OpsItemStatus)
uoiStatus = lens _uoiStatus (\s a -> s {_uoiStatus = a})

-- | Keys that you want to remove from the OperationalData map.
uoiOperationalDataToDelete :: Lens' UpdateOpsItem [Text]
uoiOperationalDataToDelete = lens _uoiOperationalDataToDelete (\s a -> s {_uoiOperationalDataToDelete = a}) . _Default . _Coerce

-- | The importance of this OpsItem in relation to other OpsItems in the system.
uoiPriority :: Lens' UpdateOpsItem (Maybe Natural)
uoiPriority = lens _uoiPriority (\s a -> s {_uoiPriority = a}) . mapping _Nat

-- | Specify a new category for an OpsItem.
uoiCategory :: Lens' UpdateOpsItem (Maybe Text)
uoiCategory = lens _uoiCategory (\s a -> s {_uoiCategory = a})

-- | Specify a new severity for an OpsItem.
uoiSeverity :: Lens' UpdateOpsItem (Maybe Text)
uoiSeverity = lens _uoiSeverity (\s a -> s {_uoiSeverity = a})

-- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
uoiRelatedOpsItems :: Lens' UpdateOpsItem [RelatedOpsItem]
uoiRelatedOpsItems = lens _uoiRelatedOpsItems (\s a -> s {_uoiRelatedOpsItems = a}) . _Default . _Coerce

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
uoiTitle :: Lens' UpdateOpsItem (Maybe Text)
uoiTitle = lens _uoiTitle (\s a -> s {_uoiTitle = a})

-- | Add new keys or edit existing key-value pairs of the OperationalData map in the OpsItem object. Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB. /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm. You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action). Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
uoiOperationalData :: Lens' UpdateOpsItem (HashMap Text (OpsItemDataValue))
uoiOperationalData = lens _uoiOperationalData (\s a -> s {_uoiOperationalData = a}) . _Default . _Map

-- | Update the information about the OpsItem. Provide enough information so that users reading this OpsItem for the first time understand the issue.
uoiDescription :: Lens' UpdateOpsItem (Maybe Text)
uoiDescription = lens _uoiDescription (\s a -> s {_uoiDescription = a})

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
uoiNotifications :: Lens' UpdateOpsItem [OpsItemNotification]
uoiNotifications = lens _uoiNotifications (\s a -> s {_uoiNotifications = a}) . _Default . _Coerce

-- | The ID of the OpsItem.
uoiOpsItemId :: Lens' UpdateOpsItem Text
uoiOpsItemId = lens _uoiOpsItemId (\s a -> s {_uoiOpsItemId = a})

instance AWSRequest UpdateOpsItem where
  type Rs UpdateOpsItem = UpdateOpsItemResponse
  request = postJSON ssm
  response =
    receiveEmpty
      (\s h x -> UpdateOpsItemResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateOpsItem

instance NFData UpdateOpsItem

instance ToHeaders UpdateOpsItem where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.UpdateOpsItem" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateOpsItem where
  toJSON UpdateOpsItem' {..} =
    object
      ( catMaybes
          [ ("Status" .=) <$> _uoiStatus,
            ("OperationalDataToDelete" .=) <$> _uoiOperationalDataToDelete,
            ("Priority" .=) <$> _uoiPriority,
            ("Category" .=) <$> _uoiCategory,
            ("Severity" .=) <$> _uoiSeverity,
            ("RelatedOpsItems" .=) <$> _uoiRelatedOpsItems,
            ("Title" .=) <$> _uoiTitle,
            ("OperationalData" .=) <$> _uoiOperationalData,
            ("Description" .=) <$> _uoiDescription,
            ("Notifications" .=) <$> _uoiNotifications,
            Just ("OpsItemId" .= _uoiOpsItemId)
          ]
      )

instance ToPath UpdateOpsItem where
  toPath = const "/"

instance ToQuery UpdateOpsItem where
  toQuery = const mempty

-- | /See:/ 'updateOpsItemResponse' smart constructor.
newtype UpdateOpsItemResponse = UpdateOpsItemResponse'
  { _uoirsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateOpsItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uoirsResponseStatus' - -- | The response status code.
updateOpsItemResponse ::
  -- | 'uoirsResponseStatus'
  Int ->
  UpdateOpsItemResponse
updateOpsItemResponse pResponseStatus_ =
  UpdateOpsItemResponse' {_uoirsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
uoirsResponseStatus :: Lens' UpdateOpsItemResponse Int
uoirsResponseStatus = lens _uoirsResponseStatus (\s a -> s {_uoirsResponseStatus = a})

instance NFData UpdateOpsItemResponse
