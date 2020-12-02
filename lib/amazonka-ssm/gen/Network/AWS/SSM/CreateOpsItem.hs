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
-- Module      : Network.AWS.SSM.CreateOpsItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new OpsItem. You must have permission in AWS Identity and Access Management (IAM) to create a new OpsItem. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
--
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.CreateOpsItem
  ( -- * Creating a Request
    createOpsItem,
    CreateOpsItem,

    -- * Request Lenses
    coiPriority,
    coiCategory,
    coiSeverity,
    coiRelatedOpsItems,
    coiOperationalData,
    coiNotifications,
    coiTags,
    coiDescription,
    coiSource,
    coiTitle,

    -- * Destructuring the Response
    createOpsItemResponse,
    CreateOpsItemResponse,

    -- * Response Lenses
    coirsOpsItemId,
    coirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'createOpsItem' smart constructor.
data CreateOpsItem = CreateOpsItem'
  { _coiPriority :: !(Maybe Nat),
    _coiCategory :: !(Maybe Text),
    _coiSeverity :: !(Maybe Text),
    _coiRelatedOpsItems :: !(Maybe [RelatedOpsItem]),
    _coiOperationalData :: !(Maybe (Map Text (OpsItemDataValue))),
    _coiNotifications :: !(Maybe [OpsItemNotification]),
    _coiTags :: !(Maybe [Tag]),
    _coiDescription :: !Text,
    _coiSource :: !Text,
    _coiTitle :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateOpsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coiPriority' - The importance of this OpsItem in relation to other OpsItems in the system.
--
-- * 'coiCategory' - Specify a category to assign to an OpsItem.
--
-- * 'coiSeverity' - Specify a severity to assign to an OpsItem.
--
-- * 'coiRelatedOpsItems' - One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- * 'coiOperationalData' - Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB. /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm. You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action). Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
--
-- * 'coiNotifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- * 'coiTags' - Optional metadata that you assign to a resource. You can restrict access to OpsItems by using an inline IAM policy that specifies tags. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ . Tags use a key-value pair. For example: @Key=Department,Value=Finance@
--
-- * 'coiDescription' - Information about the OpsItem.
--
-- * 'coiSource' - The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
--
-- * 'coiTitle' - A short heading that describes the nature of the OpsItem and the impacted resource.
createOpsItem ::
  -- | 'coiDescription'
  Text ->
  -- | 'coiSource'
  Text ->
  -- | 'coiTitle'
  Text ->
  CreateOpsItem
createOpsItem pDescription_ pSource_ pTitle_ =
  CreateOpsItem'
    { _coiPriority = Nothing,
      _coiCategory = Nothing,
      _coiSeverity = Nothing,
      _coiRelatedOpsItems = Nothing,
      _coiOperationalData = Nothing,
      _coiNotifications = Nothing,
      _coiTags = Nothing,
      _coiDescription = pDescription_,
      _coiSource = pSource_,
      _coiTitle = pTitle_
    }

-- | The importance of this OpsItem in relation to other OpsItems in the system.
coiPriority :: Lens' CreateOpsItem (Maybe Natural)
coiPriority = lens _coiPriority (\s a -> s {_coiPriority = a}) . mapping _Nat

-- | Specify a category to assign to an OpsItem.
coiCategory :: Lens' CreateOpsItem (Maybe Text)
coiCategory = lens _coiCategory (\s a -> s {_coiCategory = a})

-- | Specify a severity to assign to an OpsItem.
coiSeverity :: Lens' CreateOpsItem (Maybe Text)
coiSeverity = lens _coiSeverity (\s a -> s {_coiSeverity = a})

-- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
coiRelatedOpsItems :: Lens' CreateOpsItem [RelatedOpsItem]
coiRelatedOpsItems = lens _coiRelatedOpsItems (\s a -> s {_coiRelatedOpsItems = a}) . _Default . _Coerce

-- | Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB. /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm. You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action). Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
coiOperationalData :: Lens' CreateOpsItem (HashMap Text (OpsItemDataValue))
coiOperationalData = lens _coiOperationalData (\s a -> s {_coiOperationalData = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
coiNotifications :: Lens' CreateOpsItem [OpsItemNotification]
coiNotifications = lens _coiNotifications (\s a -> s {_coiNotifications = a}) . _Default . _Coerce

-- | Optional metadata that you assign to a resource. You can restrict access to OpsItems by using an inline IAM policy that specifies tags. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ . Tags use a key-value pair. For example: @Key=Department,Value=Finance@
coiTags :: Lens' CreateOpsItem [Tag]
coiTags = lens _coiTags (\s a -> s {_coiTags = a}) . _Default . _Coerce

-- | Information about the OpsItem.
coiDescription :: Lens' CreateOpsItem Text
coiDescription = lens _coiDescription (\s a -> s {_coiDescription = a})

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
coiSource :: Lens' CreateOpsItem Text
coiSource = lens _coiSource (\s a -> s {_coiSource = a})

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
coiTitle :: Lens' CreateOpsItem Text
coiTitle = lens _coiTitle (\s a -> s {_coiTitle = a})

instance AWSRequest CreateOpsItem where
  type Rs CreateOpsItem = CreateOpsItemResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          CreateOpsItemResponse'
            <$> (x .?> "OpsItemId") <*> (pure (fromEnum s))
      )

instance Hashable CreateOpsItem

instance NFData CreateOpsItem

instance ToHeaders CreateOpsItem where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.CreateOpsItem" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateOpsItem where
  toJSON CreateOpsItem' {..} =
    object
      ( catMaybes
          [ ("Priority" .=) <$> _coiPriority,
            ("Category" .=) <$> _coiCategory,
            ("Severity" .=) <$> _coiSeverity,
            ("RelatedOpsItems" .=) <$> _coiRelatedOpsItems,
            ("OperationalData" .=) <$> _coiOperationalData,
            ("Notifications" .=) <$> _coiNotifications,
            ("Tags" .=) <$> _coiTags,
            Just ("Description" .= _coiDescription),
            Just ("Source" .= _coiSource),
            Just ("Title" .= _coiTitle)
          ]
      )

instance ToPath CreateOpsItem where
  toPath = const "/"

instance ToQuery CreateOpsItem where
  toQuery = const mempty

-- | /See:/ 'createOpsItemResponse' smart constructor.
data CreateOpsItemResponse = CreateOpsItemResponse'
  { _coirsOpsItemId ::
      !(Maybe Text),
    _coirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateOpsItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coirsOpsItemId' - The ID of the OpsItem.
--
-- * 'coirsResponseStatus' - -- | The response status code.
createOpsItemResponse ::
  -- | 'coirsResponseStatus'
  Int ->
  CreateOpsItemResponse
createOpsItemResponse pResponseStatus_ =
  CreateOpsItemResponse'
    { _coirsOpsItemId = Nothing,
      _coirsResponseStatus = pResponseStatus_
    }

-- | The ID of the OpsItem.
coirsOpsItemId :: Lens' CreateOpsItemResponse (Maybe Text)
coirsOpsItemId = lens _coirsOpsItemId (\s a -> s {_coirsOpsItemId = a})

-- | -- | The response status code.
coirsResponseStatus :: Lens' CreateOpsItemResponse Int
coirsResponseStatus = lens _coirsResponseStatus (\s a -> s {_coirsResponseStatus = a})

instance NFData CreateOpsItemResponse
