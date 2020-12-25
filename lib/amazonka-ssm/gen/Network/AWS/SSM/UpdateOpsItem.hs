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
    uoiCategory,
    uoiDescription,
    uoiNotifications,
    uoiOperationalData,
    uoiOperationalDataToDelete,
    uoiPriority,
    uoiRelatedOpsItems,
    uoiSeverity,
    uoiStatus,
    uoiTitle,

    -- * Destructuring the response
    UpdateOpsItemResponse (..),
    mkUpdateOpsItemResponse,

    -- ** Response lenses
    uoirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateOpsItem' smart constructor.
data UpdateOpsItem = UpdateOpsItem'
  { -- | The ID of the OpsItem.
    opsItemId :: Types.OpsItemId,
    -- | Specify a new category for an OpsItem.
    category :: Core.Maybe Types.Category,
    -- | Update the information about the OpsItem. Provide enough information so that users reading this OpsItem for the first time understand the issue.
    description :: Core.Maybe Types.OpsItemDescription,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
    notifications :: Core.Maybe [Types.OpsItemNotification],
    -- | Add new keys or edit existing key-value pairs of the OperationalData map in the OpsItem object.
    --
    -- Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
    -- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
    -- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
    -- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
    operationalData :: Core.Maybe (Core.HashMap Types.OpsItemDataKey Types.OpsItemDataValue),
    -- | Keys that you want to remove from the OperationalData map.
    operationalDataToDelete :: Core.Maybe [Types.String],
    -- | The importance of this OpsItem in relation to other OpsItems in the system.
    priority :: Core.Maybe Core.Natural,
    -- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
    relatedOpsItems :: Core.Maybe [Types.RelatedOpsItem],
    -- | Specify a new severity for an OpsItem.
    severity :: Core.Maybe Types.Severity,
    -- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
    status :: Core.Maybe Types.OpsItemStatus,
    -- | A short heading that describes the nature of the OpsItem and the impacted resource.
    title :: Core.Maybe Types.Title
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOpsItem' value with any optional fields omitted.
mkUpdateOpsItem ::
  -- | 'opsItemId'
  Types.OpsItemId ->
  UpdateOpsItem
mkUpdateOpsItem opsItemId =
  UpdateOpsItem'
    { opsItemId,
      category = Core.Nothing,
      description = Core.Nothing,
      notifications = Core.Nothing,
      operationalData = Core.Nothing,
      operationalDataToDelete = Core.Nothing,
      priority = Core.Nothing,
      relatedOpsItems = Core.Nothing,
      severity = Core.Nothing,
      status = Core.Nothing,
      title = Core.Nothing
    }

-- | The ID of the OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiOpsItemId :: Lens.Lens' UpdateOpsItem Types.OpsItemId
uoiOpsItemId = Lens.field @"opsItemId"
{-# DEPRECATED uoiOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

-- | Specify a new category for an OpsItem.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiCategory :: Lens.Lens' UpdateOpsItem (Core.Maybe Types.Category)
uoiCategory = Lens.field @"category"
{-# DEPRECATED uoiCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Update the information about the OpsItem. Provide enough information so that users reading this OpsItem for the first time understand the issue.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiDescription :: Lens.Lens' UpdateOpsItem (Core.Maybe Types.OpsItemDescription)
uoiDescription = Lens.field @"description"
{-# DEPRECATED uoiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiNotifications :: Lens.Lens' UpdateOpsItem (Core.Maybe [Types.OpsItemNotification])
uoiNotifications = Lens.field @"notifications"
{-# DEPRECATED uoiNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

-- | Add new keys or edit existing key-value pairs of the OperationalData map in the OpsItem object.
--
-- Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
-- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
-- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
-- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'operationalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiOperationalData :: Lens.Lens' UpdateOpsItem (Core.Maybe (Core.HashMap Types.OpsItemDataKey Types.OpsItemDataValue))
uoiOperationalData = Lens.field @"operationalData"
{-# DEPRECATED uoiOperationalData "Use generic-lens or generic-optics with 'operationalData' instead." #-}

-- | Keys that you want to remove from the OperationalData map.
--
-- /Note:/ Consider using 'operationalDataToDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiOperationalDataToDelete :: Lens.Lens' UpdateOpsItem (Core.Maybe [Types.String])
uoiOperationalDataToDelete = Lens.field @"operationalDataToDelete"
{-# DEPRECATED uoiOperationalDataToDelete "Use generic-lens or generic-optics with 'operationalDataToDelete' instead." #-}

-- | The importance of this OpsItem in relation to other OpsItems in the system.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiPriority :: Lens.Lens' UpdateOpsItem (Core.Maybe Core.Natural)
uoiPriority = Lens.field @"priority"
{-# DEPRECATED uoiPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- /Note:/ Consider using 'relatedOpsItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiRelatedOpsItems :: Lens.Lens' UpdateOpsItem (Core.Maybe [Types.RelatedOpsItem])
uoiRelatedOpsItems = Lens.field @"relatedOpsItems"
{-# DEPRECATED uoiRelatedOpsItems "Use generic-lens or generic-optics with 'relatedOpsItems' instead." #-}

-- | Specify a new severity for an OpsItem.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiSeverity :: Lens.Lens' UpdateOpsItem (Core.Maybe Types.Severity)
uoiSeverity = Lens.field @"severity"
{-# DEPRECATED uoiSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems.html#OpsCenter-working-with-OpsItems-editing-details Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiStatus :: Lens.Lens' UpdateOpsItem (Core.Maybe Types.OpsItemStatus)
uoiStatus = Lens.field @"status"
{-# DEPRECATED uoiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoiTitle :: Lens.Lens' UpdateOpsItem (Core.Maybe Types.Title)
uoiTitle = Lens.field @"title"
{-# DEPRECATED uoiTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON UpdateOpsItem where
  toJSON UpdateOpsItem {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OpsItemId" Core..= opsItemId),
            ("Category" Core..=) Core.<$> category,
            ("Description" Core..=) Core.<$> description,
            ("Notifications" Core..=) Core.<$> notifications,
            ("OperationalData" Core..=) Core.<$> operationalData,
            ("OperationalDataToDelete" Core..=)
              Core.<$> operationalDataToDelete,
            ("Priority" Core..=) Core.<$> priority,
            ("RelatedOpsItems" Core..=) Core.<$> relatedOpsItems,
            ("Severity" Core..=) Core.<$> severity,
            ("Status" Core..=) Core.<$> status,
            ("Title" Core..=) Core.<$> title
          ]
      )

instance Core.AWSRequest UpdateOpsItem where
  type Rs UpdateOpsItem = UpdateOpsItemResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.UpdateOpsItem")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateOpsItemResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateOpsItemResponse' smart constructor.
newtype UpdateOpsItemResponse = UpdateOpsItemResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateOpsItemResponse' value with any optional fields omitted.
mkUpdateOpsItemResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateOpsItemResponse
mkUpdateOpsItemResponse responseStatus =
  UpdateOpsItemResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoirrsResponseStatus :: Lens.Lens' UpdateOpsItemResponse Core.Int
uoirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uoirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
