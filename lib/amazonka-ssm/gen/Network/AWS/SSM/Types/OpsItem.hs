{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItem
  ( OpsItem (..),

    -- * Smart constructor
    mkOpsItem,

    -- * Lenses
    oiCategory,
    oiCreatedBy,
    oiCreatedTime,
    oiDescription,
    oiLastModifiedBy,
    oiLastModifiedTime,
    oiNotifications,
    oiOperationalData,
    oiOpsItemId,
    oiPriority,
    oiRelatedOpsItems,
    oiSeverity,
    oiSource,
    oiStatus,
    oiTitle,
    oiVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OpsItemCategory as Types
import qualified Network.AWS.SSM.Types.OpsItemDataKey as Types
import qualified Network.AWS.SSM.Types.OpsItemDataValue as Types
import qualified Network.AWS.SSM.Types.OpsItemDescription as Types
import qualified Network.AWS.SSM.Types.OpsItemId as Types
import qualified Network.AWS.SSM.Types.OpsItemNotification as Types
import qualified Network.AWS.SSM.Types.OpsItemSeverity as Types
import qualified Network.AWS.SSM.Types.OpsItemSource as Types
import qualified Network.AWS.SSM.Types.OpsItemStatus as Types
import qualified Network.AWS.SSM.Types.RelatedOpsItem as Types
import qualified Network.AWS.SSM.Types.String as Types
import qualified Network.AWS.SSM.Types.Title as Types

-- | Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- /See:/ 'mkOpsItem' smart constructor.
data OpsItem = OpsItem'
  { -- | An OpsItem category. Category options include: Availability, Cost, Performance, Recovery, Security.
    category :: Core.Maybe Types.OpsItemCategory,
    -- | The ARN of the AWS account that created the OpsItem.
    createdBy :: Core.Maybe Types.String,
    -- | The date and time the OpsItem was created.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | The OpsItem description.
    description :: Core.Maybe Types.OpsItemDescription,
    -- | The ARN of the AWS account that last updated the OpsItem.
    lastModifiedBy :: Core.Maybe Types.String,
    -- | The date and time the OpsItem was last updated.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
    notifications :: Core.Maybe [Types.OpsItemNotification],
    -- | Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
    --
    -- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
    -- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
    -- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
    operationalData :: Core.Maybe (Core.HashMap Types.OpsItemDataKey Types.OpsItemDataValue),
    -- | The ID of the OpsItem.
    opsItemId :: Core.Maybe Types.OpsItemId,
    -- | The importance of this OpsItem in relation to other OpsItems in the system.
    priority :: Core.Maybe Core.Natural,
    -- | One or more OpsItems that share something in common with the current OpsItem. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
    relatedOpsItems :: Core.Maybe [Types.RelatedOpsItem],
    -- | The severity of the OpsItem. Severity options range from 1 to 4.
    severity :: Core.Maybe Types.OpsItemSeverity,
    -- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The impacted resource is a subset of source.
    source :: Core.Maybe Types.OpsItemSource,
    -- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
    status :: Core.Maybe Types.OpsItemStatus,
    -- | A short heading that describes the nature of the OpsItem and the impacted resource.
    title :: Core.Maybe Types.Title,
    -- | The version of this OpsItem. Each time the OpsItem is edited the version number increments by one.
    version :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OpsItem' value with any optional fields omitted.
mkOpsItem ::
  OpsItem
mkOpsItem =
  OpsItem'
    { category = Core.Nothing,
      createdBy = Core.Nothing,
      createdTime = Core.Nothing,
      description = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      notifications = Core.Nothing,
      operationalData = Core.Nothing,
      opsItemId = Core.Nothing,
      priority = Core.Nothing,
      relatedOpsItems = Core.Nothing,
      severity = Core.Nothing,
      source = Core.Nothing,
      status = Core.Nothing,
      title = Core.Nothing,
      version = Core.Nothing
    }

-- | An OpsItem category. Category options include: Availability, Cost, Performance, Recovery, Security.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiCategory :: Lens.Lens' OpsItem (Core.Maybe Types.OpsItemCategory)
oiCategory = Lens.field @"category"
{-# DEPRECATED oiCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The ARN of the AWS account that created the OpsItem.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiCreatedBy :: Lens.Lens' OpsItem (Core.Maybe Types.String)
oiCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED oiCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The date and time the OpsItem was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiCreatedTime :: Lens.Lens' OpsItem (Core.Maybe Core.NominalDiffTime)
oiCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED oiCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The OpsItem description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiDescription :: Lens.Lens' OpsItem (Core.Maybe Types.OpsItemDescription)
oiDescription = Lens.field @"description"
{-# DEPRECATED oiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the AWS account that last updated the OpsItem.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiLastModifiedBy :: Lens.Lens' OpsItem (Core.Maybe Types.String)
oiLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED oiLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The date and time the OpsItem was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiLastModifiedTime :: Lens.Lens' OpsItem (Core.Maybe Core.NominalDiffTime)
oiLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED oiLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiNotifications :: Lens.Lens' OpsItem (Core.Maybe [Types.OpsItemNotification])
oiNotifications = Lens.field @"notifications"
{-# DEPRECATED oiNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

-- | Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
--
-- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
-- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
-- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'operationalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiOperationalData :: Lens.Lens' OpsItem (Core.Maybe (Core.HashMap Types.OpsItemDataKey Types.OpsItemDataValue))
oiOperationalData = Lens.field @"operationalData"
{-# DEPRECATED oiOperationalData "Use generic-lens or generic-optics with 'operationalData' instead." #-}

-- | The ID of the OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiOpsItemId :: Lens.Lens' OpsItem (Core.Maybe Types.OpsItemId)
oiOpsItemId = Lens.field @"opsItemId"
{-# DEPRECATED oiOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

-- | The importance of this OpsItem in relation to other OpsItems in the system.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiPriority :: Lens.Lens' OpsItem (Core.Maybe Core.Natural)
oiPriority = Lens.field @"priority"
{-# DEPRECATED oiPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | One or more OpsItems that share something in common with the current OpsItem. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- /Note:/ Consider using 'relatedOpsItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiRelatedOpsItems :: Lens.Lens' OpsItem (Core.Maybe [Types.RelatedOpsItem])
oiRelatedOpsItems = Lens.field @"relatedOpsItems"
{-# DEPRECATED oiRelatedOpsItems "Use generic-lens or generic-optics with 'relatedOpsItems' instead." #-}

-- | The severity of the OpsItem. Severity options range from 1 to 4.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiSeverity :: Lens.Lens' OpsItem (Core.Maybe Types.OpsItemSeverity)
oiSeverity = Lens.field @"severity"
{-# DEPRECATED oiSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The impacted resource is a subset of source.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiSource :: Lens.Lens' OpsItem (Core.Maybe Types.OpsItemSource)
oiSource = Lens.field @"source"
{-# DEPRECATED oiSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiStatus :: Lens.Lens' OpsItem (Core.Maybe Types.OpsItemStatus)
oiStatus = Lens.field @"status"
{-# DEPRECATED oiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiTitle :: Lens.Lens' OpsItem (Core.Maybe Types.Title)
oiTitle = Lens.field @"title"
{-# DEPRECATED oiTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The version of this OpsItem. Each time the OpsItem is edited the version number increments by one.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiVersion :: Lens.Lens' OpsItem (Core.Maybe Types.String)
oiVersion = Lens.field @"version"
{-# DEPRECATED oiVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON OpsItem where
  parseJSON =
    Core.withObject "OpsItem" Core.$
      \x ->
        OpsItem'
          Core.<$> (x Core..:? "Category")
          Core.<*> (x Core..:? "CreatedBy")
          Core.<*> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "LastModifiedBy")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "Notifications")
          Core.<*> (x Core..:? "OperationalData")
          Core.<*> (x Core..:? "OpsItemId")
          Core.<*> (x Core..:? "Priority")
          Core.<*> (x Core..:? "RelatedOpsItems")
          Core.<*> (x Core..:? "Severity")
          Core.<*> (x Core..:? "Source")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Title")
          Core.<*> (x Core..:? "Version")
