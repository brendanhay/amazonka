{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.UpdateOpsItem
  ( -- * Creating a request
    UpdateOpsItem (..),
    mkUpdateOpsItem,

    -- ** Request lenses
    uoiOpsItemId,
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

    -- * Destructuring the response
    UpdateOpsItemResponse (..),
    mkUpdateOpsItemResponse,

    -- ** Response lenses
    uoirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdateOpsItem' smart constructor.
data UpdateOpsItem = UpdateOpsItem'
  { -- | The ID of the OpsItem.
    opsItemId :: Lude.Text,
    -- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
    status :: Lude.Maybe OpsItemStatus,
    -- | Keys that you want to remove from the OperationalData map.
    operationalDataToDelete :: Lude.Maybe [Lude.Text],
    -- | The importance of this OpsItem in relation to other OpsItems in the system.
    priority :: Lude.Maybe Lude.Natural,
    -- | Specify a new category for an OpsItem.
    category :: Lude.Maybe Lude.Text,
    -- | Specify a new severity for an OpsItem.
    severity :: Lude.Maybe Lude.Text,
    -- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
    relatedOpsItems :: Lude.Maybe [RelatedOpsItem],
    -- | A short heading that describes the nature of the OpsItem and the impacted resource.
    title :: Lude.Maybe Lude.Text,
    -- | Add new keys or edit existing key-value pairs of the OperationalData map in the OpsItem object.
    --
    -- Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
    -- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
    -- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
    -- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
    operationalData :: Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue)),
    -- | Update the information about the OpsItem. Provide enough information so that users reading this OpsItem for the first time understand the issue.
    description :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
    notifications :: Lude.Maybe [OpsItemNotification]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateOpsItem' with the minimum fields required to make a request.
--
-- * 'opsItemId' - The ID of the OpsItem.
-- * 'status' - The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
-- * 'operationalDataToDelete' - Keys that you want to remove from the OperationalData map.
-- * 'priority' - The importance of this OpsItem in relation to other OpsItems in the system.
-- * 'category' - Specify a new category for an OpsItem.
-- * 'severity' - Specify a new severity for an OpsItem.
-- * 'relatedOpsItems' - One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
-- * 'title' - A short heading that describes the nature of the OpsItem and the impacted resource.
-- * 'operationalData' - Add new keys or edit existing key-value pairs of the OperationalData map in the OpsItem object.
--
-- Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
-- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
-- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
-- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
-- * 'description' - Update the information about the OpsItem. Provide enough information so that users reading this OpsItem for the first time understand the issue.
-- * 'notifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
mkUpdateOpsItem ::
  -- | 'opsItemId'
  Lude.Text ->
  UpdateOpsItem
mkUpdateOpsItem pOpsItemId_ =
  UpdateOpsItem'
    { opsItemId = pOpsItemId_,
      status = Lude.Nothing,
      operationalDataToDelete = Lude.Nothing,
      priority = Lude.Nothing,
      category = Lude.Nothing,
      severity = Lude.Nothing,
      relatedOpsItems = Lude.Nothing,
      title = Lude.Nothing,
      operationalData = Lude.Nothing,
      description = Lude.Nothing,
      notifications = Lude.Nothing
    }

-- | The ID of the OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiOpsItemId :: Lens.Lens' UpdateOpsItem Lude.Text
uoiOpsItemId = Lens.lens (opsItemId :: UpdateOpsItem -> Lude.Text) (\s a -> s {opsItemId = a} :: UpdateOpsItem)
{-# DEPRECATED uoiOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

-- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiStatus :: Lens.Lens' UpdateOpsItem (Lude.Maybe OpsItemStatus)
uoiStatus = Lens.lens (status :: UpdateOpsItem -> Lude.Maybe OpsItemStatus) (\s a -> s {status = a} :: UpdateOpsItem)
{-# DEPRECATED uoiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Keys that you want to remove from the OperationalData map.
--
-- /Note:/ Consider using 'operationalDataToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiOperationalDataToDelete :: Lens.Lens' UpdateOpsItem (Lude.Maybe [Lude.Text])
uoiOperationalDataToDelete = Lens.lens (operationalDataToDelete :: UpdateOpsItem -> Lude.Maybe [Lude.Text]) (\s a -> s {operationalDataToDelete = a} :: UpdateOpsItem)
{-# DEPRECATED uoiOperationalDataToDelete "Use generic-lens or generic-optics with 'operationalDataToDelete' instead." #-}

-- | The importance of this OpsItem in relation to other OpsItems in the system.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiPriority :: Lens.Lens' UpdateOpsItem (Lude.Maybe Lude.Natural)
uoiPriority = Lens.lens (priority :: UpdateOpsItem -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: UpdateOpsItem)
{-# DEPRECATED uoiPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Specify a new category for an OpsItem.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiCategory :: Lens.Lens' UpdateOpsItem (Lude.Maybe Lude.Text)
uoiCategory = Lens.lens (category :: UpdateOpsItem -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: UpdateOpsItem)
{-# DEPRECATED uoiCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Specify a new severity for an OpsItem.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiSeverity :: Lens.Lens' UpdateOpsItem (Lude.Maybe Lude.Text)
uoiSeverity = Lens.lens (severity :: UpdateOpsItem -> Lude.Maybe Lude.Text) (\s a -> s {severity = a} :: UpdateOpsItem)
{-# DEPRECATED uoiSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- /Note:/ Consider using 'relatedOpsItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiRelatedOpsItems :: Lens.Lens' UpdateOpsItem (Lude.Maybe [RelatedOpsItem])
uoiRelatedOpsItems = Lens.lens (relatedOpsItems :: UpdateOpsItem -> Lude.Maybe [RelatedOpsItem]) (\s a -> s {relatedOpsItems = a} :: UpdateOpsItem)
{-# DEPRECATED uoiRelatedOpsItems "Use generic-lens or generic-optics with 'relatedOpsItems' instead." #-}

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiTitle :: Lens.Lens' UpdateOpsItem (Lude.Maybe Lude.Text)
uoiTitle = Lens.lens (title :: UpdateOpsItem -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: UpdateOpsItem)
{-# DEPRECATED uoiTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | Add new keys or edit existing key-value pairs of the OperationalData map in the OpsItem object.
--
-- Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
-- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
-- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
-- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'operationalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiOperationalData :: Lens.Lens' UpdateOpsItem (Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue)))
uoiOperationalData = Lens.lens (operationalData :: UpdateOpsItem -> Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue))) (\s a -> s {operationalData = a} :: UpdateOpsItem)
{-# DEPRECATED uoiOperationalData "Use generic-lens or generic-optics with 'operationalData' instead." #-}

-- | Update the information about the OpsItem. Provide enough information so that users reading this OpsItem for the first time understand the issue.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiDescription :: Lens.Lens' UpdateOpsItem (Lude.Maybe Lude.Text)
uoiDescription = Lens.lens (description :: UpdateOpsItem -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateOpsItem)
{-# DEPRECATED uoiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiNotifications :: Lens.Lens' UpdateOpsItem (Lude.Maybe [OpsItemNotification])
uoiNotifications = Lens.lens (notifications :: UpdateOpsItem -> Lude.Maybe [OpsItemNotification]) (\s a -> s {notifications = a} :: UpdateOpsItem)
{-# DEPRECATED uoiNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

instance Lude.AWSRequest UpdateOpsItem where
  type Rs UpdateOpsItem = UpdateOpsItemResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateOpsItemResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateOpsItem where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdateOpsItem" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateOpsItem where
  toJSON UpdateOpsItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OpsItemId" Lude..= opsItemId),
            ("Status" Lude..=) Lude.<$> status,
            ("OperationalDataToDelete" Lude..=)
              Lude.<$> operationalDataToDelete,
            ("Priority" Lude..=) Lude.<$> priority,
            ("Category" Lude..=) Lude.<$> category,
            ("Severity" Lude..=) Lude.<$> severity,
            ("RelatedOpsItems" Lude..=) Lude.<$> relatedOpsItems,
            ("Title" Lude..=) Lude.<$> title,
            ("OperationalData" Lude..=) Lude.<$> operationalData,
            ("Description" Lude..=) Lude.<$> description,
            ("Notifications" Lude..=) Lude.<$> notifications
          ]
      )

instance Lude.ToPath UpdateOpsItem where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateOpsItem where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateOpsItemResponse' smart constructor.
newtype UpdateOpsItemResponse = UpdateOpsItemResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateOpsItemResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateOpsItemResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateOpsItemResponse
mkUpdateOpsItemResponse pResponseStatus_ =
  UpdateOpsItemResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoirsResponseStatus :: Lens.Lens' UpdateOpsItemResponse Lude.Int
uoirsResponseStatus = Lens.lens (responseStatus :: UpdateOpsItemResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateOpsItemResponse)
{-# DEPRECATED uoirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
