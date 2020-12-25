{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.CreateOpsItem
  ( -- * Creating a request
    CreateOpsItem (..),
    mkCreateOpsItem,

    -- ** Request lenses
    coiDescription,
    coiSource,
    coiTitle,
    coiCategory,
    coiNotifications,
    coiOperationalData,
    coiPriority,
    coiRelatedOpsItems,
    coiSeverity,
    coiTags,

    -- * Destructuring the response
    CreateOpsItemResponse (..),
    mkCreateOpsItemResponse,

    -- ** Response lenses
    coirrsOpsItemId,
    coirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCreateOpsItem' smart constructor.
data CreateOpsItem = CreateOpsItem'
  { -- | Information about the OpsItem.
    description :: Types.Description,
    -- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
    source :: Types.Source,
    -- | A short heading that describes the nature of the OpsItem and the impacted resource.
    title :: Types.Title,
    -- | Specify a category to assign to an OpsItem.
    category :: Core.Maybe Types.Category,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
    notifications :: Core.Maybe [Types.OpsItemNotification],
    -- | Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
    --
    -- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
    -- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
    -- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
    operationalData :: Core.Maybe (Core.HashMap Types.OpsItemDataKey Types.OpsItemDataValue),
    -- | The importance of this OpsItem in relation to other OpsItems in the system.
    priority :: Core.Maybe Core.Natural,
    -- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
    relatedOpsItems :: Core.Maybe [Types.RelatedOpsItem],
    -- | Specify a severity to assign to an OpsItem.
    severity :: Core.Maybe Types.Severity,
    -- | Optional metadata that you assign to a resource. You can restrict access to OpsItems by using an inline IAM policy that specifies tags. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
    --
    -- Tags use a key-value pair. For example:
    -- @Key=Department,Value=Finance@
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOpsItem' value with any optional fields omitted.
mkCreateOpsItem ::
  -- | 'description'
  Types.Description ->
  -- | 'source'
  Types.Source ->
  -- | 'title'
  Types.Title ->
  CreateOpsItem
mkCreateOpsItem description source title =
  CreateOpsItem'
    { description,
      source,
      title,
      category = Core.Nothing,
      notifications = Core.Nothing,
      operationalData = Core.Nothing,
      priority = Core.Nothing,
      relatedOpsItems = Core.Nothing,
      severity = Core.Nothing,
      tags = Core.Nothing
    }

-- | Information about the OpsItem.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiDescription :: Lens.Lens' CreateOpsItem Types.Description
coiDescription = Lens.field @"description"
{-# DEPRECATED coiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiSource :: Lens.Lens' CreateOpsItem Types.Source
coiSource = Lens.field @"source"
{-# DEPRECATED coiSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiTitle :: Lens.Lens' CreateOpsItem Types.Title
coiTitle = Lens.field @"title"
{-# DEPRECATED coiTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | Specify a category to assign to an OpsItem.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiCategory :: Lens.Lens' CreateOpsItem (Core.Maybe Types.Category)
coiCategory = Lens.field @"category"
{-# DEPRECATED coiCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiNotifications :: Lens.Lens' CreateOpsItem (Core.Maybe [Types.OpsItemNotification])
coiNotifications = Lens.field @"notifications"
{-# DEPRECATED coiNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

-- | Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
--
-- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
-- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
-- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'operationalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiOperationalData :: Lens.Lens' CreateOpsItem (Core.Maybe (Core.HashMap Types.OpsItemDataKey Types.OpsItemDataValue))
coiOperationalData = Lens.field @"operationalData"
{-# DEPRECATED coiOperationalData "Use generic-lens or generic-optics with 'operationalData' instead." #-}

-- | The importance of this OpsItem in relation to other OpsItems in the system.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiPriority :: Lens.Lens' CreateOpsItem (Core.Maybe Core.Natural)
coiPriority = Lens.field @"priority"
{-# DEPRECATED coiPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- /Note:/ Consider using 'relatedOpsItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiRelatedOpsItems :: Lens.Lens' CreateOpsItem (Core.Maybe [Types.RelatedOpsItem])
coiRelatedOpsItems = Lens.field @"relatedOpsItems"
{-# DEPRECATED coiRelatedOpsItems "Use generic-lens or generic-optics with 'relatedOpsItems' instead." #-}

-- | Specify a severity to assign to an OpsItem.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiSeverity :: Lens.Lens' CreateOpsItem (Core.Maybe Types.Severity)
coiSeverity = Lens.field @"severity"
{-# DEPRECATED coiSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | Optional metadata that you assign to a resource. You can restrict access to OpsItems by using an inline IAM policy that specifies tags. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- Tags use a key-value pair. For example:
-- @Key=Department,Value=Finance@
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiTags :: Lens.Lens' CreateOpsItem (Core.Maybe [Types.Tag])
coiTags = Lens.field @"tags"
{-# DEPRECATED coiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateOpsItem where
  toJSON CreateOpsItem {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Description" Core..= description),
            Core.Just ("Source" Core..= source),
            Core.Just ("Title" Core..= title),
            ("Category" Core..=) Core.<$> category,
            ("Notifications" Core..=) Core.<$> notifications,
            ("OperationalData" Core..=) Core.<$> operationalData,
            ("Priority" Core..=) Core.<$> priority,
            ("RelatedOpsItems" Core..=) Core.<$> relatedOpsItems,
            ("Severity" Core..=) Core.<$> severity,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateOpsItem where
  type Rs CreateOpsItem = CreateOpsItemResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.CreateOpsItem")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOpsItemResponse'
            Core.<$> (x Core..:? "OpsItemId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateOpsItemResponse' smart constructor.
data CreateOpsItemResponse = CreateOpsItemResponse'
  { -- | The ID of the OpsItem.
    opsItemId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateOpsItemResponse' value with any optional fields omitted.
mkCreateOpsItemResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateOpsItemResponse
mkCreateOpsItemResponse responseStatus =
  CreateOpsItemResponse' {opsItemId = Core.Nothing, responseStatus}

-- | The ID of the OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coirrsOpsItemId :: Lens.Lens' CreateOpsItemResponse (Core.Maybe Types.String)
coirrsOpsItemId = Lens.field @"opsItemId"
{-# DEPRECATED coirrsOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coirrsResponseStatus :: Lens.Lens' CreateOpsItemResponse Core.Int
coirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED coirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
