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
    oiOpsItemId,
    oiStatus,
    oiPriority,
    oiCreatedTime,
    oiCategory,
    oiSeverity,
    oiCreatedBy,
    oiLastModifiedTime,
    oiVersion,
    oiSource,
    oiRelatedOpsItems,
    oiTitle,
    oiLastModifiedBy,
    oiOperationalData,
    oiDescription,
    oiNotifications,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.OpsItemDataValue
import Network.AWS.SSM.Types.OpsItemNotification
import Network.AWS.SSM.Types.OpsItemStatus
import Network.AWS.SSM.Types.RelatedOpsItem

-- | Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- /See:/ 'mkOpsItem' smart constructor.
data OpsItem = OpsItem'
  { opsItemId :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe OpsItemStatus,
    priority :: Lude.Maybe Lude.Natural,
    createdTime :: Lude.Maybe Lude.Timestamp,
    category :: Lude.Maybe Lude.Text,
    severity :: Lude.Maybe Lude.Text,
    createdBy :: Lude.Maybe Lude.Text,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    version :: Lude.Maybe Lude.Text,
    source :: Lude.Maybe Lude.Text,
    relatedOpsItems :: Lude.Maybe [RelatedOpsItem],
    title :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe Lude.Text,
    operationalData ::
      Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue)),
    description :: Lude.Maybe Lude.Text,
    notifications :: Lude.Maybe [OpsItemNotification]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsItem' with the minimum fields required to make a request.
--
-- * 'category' - An OpsItem category. Category options include: Availability, Cost, Performance, Recovery, Security.
-- * 'createdBy' - The ARN of the AWS account that created the OpsItem.
-- * 'createdTime' - The date and time the OpsItem was created.
-- * 'description' - The OpsItem description.
-- * 'lastModifiedBy' - The ARN of the AWS account that last updated the OpsItem.
-- * 'lastModifiedTime' - The date and time the OpsItem was last updated.
-- * 'notifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
-- * 'operationalData' - Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
--
-- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
-- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
-- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
-- * 'opsItemId' - The ID of the OpsItem.
-- * 'priority' - The importance of this OpsItem in relation to other OpsItems in the system.
-- * 'relatedOpsItems' - One or more OpsItems that share something in common with the current OpsItem. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
-- * 'severity' - The severity of the OpsItem. Severity options range from 1 to 4.
-- * 'source' - The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The impacted resource is a subset of source.
-- * 'status' - The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
-- * 'title' - A short heading that describes the nature of the OpsItem and the impacted resource.
-- * 'version' - The version of this OpsItem. Each time the OpsItem is edited the version number increments by one.
mkOpsItem ::
  OpsItem
mkOpsItem =
  OpsItem'
    { opsItemId = Lude.Nothing,
      status = Lude.Nothing,
      priority = Lude.Nothing,
      createdTime = Lude.Nothing,
      category = Lude.Nothing,
      severity = Lude.Nothing,
      createdBy = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      version = Lude.Nothing,
      source = Lude.Nothing,
      relatedOpsItems = Lude.Nothing,
      title = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      operationalData = Lude.Nothing,
      description = Lude.Nothing,
      notifications = Lude.Nothing
    }

-- | The ID of the OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiOpsItemId :: Lens.Lens' OpsItem (Lude.Maybe Lude.Text)
oiOpsItemId = Lens.lens (opsItemId :: OpsItem -> Lude.Maybe Lude.Text) (\s a -> s {opsItemId = a} :: OpsItem)
{-# DEPRECATED oiOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

-- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ . For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-working-with-OpsItems-editing-details.html Editing OpsItem details> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiStatus :: Lens.Lens' OpsItem (Lude.Maybe OpsItemStatus)
oiStatus = Lens.lens (status :: OpsItem -> Lude.Maybe OpsItemStatus) (\s a -> s {status = a} :: OpsItem)
{-# DEPRECATED oiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The importance of this OpsItem in relation to other OpsItems in the system.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiPriority :: Lens.Lens' OpsItem (Lude.Maybe Lude.Natural)
oiPriority = Lens.lens (priority :: OpsItem -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: OpsItem)
{-# DEPRECATED oiPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The date and time the OpsItem was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiCreatedTime :: Lens.Lens' OpsItem (Lude.Maybe Lude.Timestamp)
oiCreatedTime = Lens.lens (createdTime :: OpsItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: OpsItem)
{-# DEPRECATED oiCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | An OpsItem category. Category options include: Availability, Cost, Performance, Recovery, Security.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiCategory :: Lens.Lens' OpsItem (Lude.Maybe Lude.Text)
oiCategory = Lens.lens (category :: OpsItem -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: OpsItem)
{-# DEPRECATED oiCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The severity of the OpsItem. Severity options range from 1 to 4.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiSeverity :: Lens.Lens' OpsItem (Lude.Maybe Lude.Text)
oiSeverity = Lens.lens (severity :: OpsItem -> Lude.Maybe Lude.Text) (\s a -> s {severity = a} :: OpsItem)
{-# DEPRECATED oiSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The ARN of the AWS account that created the OpsItem.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiCreatedBy :: Lens.Lens' OpsItem (Lude.Maybe Lude.Text)
oiCreatedBy = Lens.lens (createdBy :: OpsItem -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: OpsItem)
{-# DEPRECATED oiCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The date and time the OpsItem was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiLastModifiedTime :: Lens.Lens' OpsItem (Lude.Maybe Lude.Timestamp)
oiLastModifiedTime = Lens.lens (lastModifiedTime :: OpsItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: OpsItem)
{-# DEPRECATED oiLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The version of this OpsItem. Each time the OpsItem is edited the version number increments by one.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiVersion :: Lens.Lens' OpsItem (Lude.Maybe Lude.Text)
oiVersion = Lens.lens (version :: OpsItem -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: OpsItem)
{-# DEPRECATED oiVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager. The impacted resource is a subset of source.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiSource :: Lens.Lens' OpsItem (Lude.Maybe Lude.Text)
oiSource = Lens.lens (source :: OpsItem -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: OpsItem)
{-# DEPRECATED oiSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | One or more OpsItems that share something in common with the current OpsItem. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- /Note:/ Consider using 'relatedOpsItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiRelatedOpsItems :: Lens.Lens' OpsItem (Lude.Maybe [RelatedOpsItem])
oiRelatedOpsItems = Lens.lens (relatedOpsItems :: OpsItem -> Lude.Maybe [RelatedOpsItem]) (\s a -> s {relatedOpsItems = a} :: OpsItem)
{-# DEPRECATED oiRelatedOpsItems "Use generic-lens or generic-optics with 'relatedOpsItems' instead." #-}

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiTitle :: Lens.Lens' OpsItem (Lude.Maybe Lude.Text)
oiTitle = Lens.lens (title :: OpsItem -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: OpsItem)
{-# DEPRECATED oiTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The ARN of the AWS account that last updated the OpsItem.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiLastModifiedBy :: Lens.Lens' OpsItem (Lude.Maybe Lude.Text)
oiLastModifiedBy = Lens.lens (lastModifiedBy :: OpsItem -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: OpsItem)
{-# DEPRECATED oiLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
--
-- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
-- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
-- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'operationalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiOperationalData :: Lens.Lens' OpsItem (Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue)))
oiOperationalData = Lens.lens (operationalData :: OpsItem -> Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue))) (\s a -> s {operationalData = a} :: OpsItem)
{-# DEPRECATED oiOperationalData "Use generic-lens or generic-optics with 'operationalData' instead." #-}

-- | The OpsItem description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiDescription :: Lens.Lens' OpsItem (Lude.Maybe Lude.Text)
oiDescription = Lens.lens (description :: OpsItem -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: OpsItem)
{-# DEPRECATED oiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiNotifications :: Lens.Lens' OpsItem (Lude.Maybe [OpsItemNotification])
oiNotifications = Lens.lens (notifications :: OpsItem -> Lude.Maybe [OpsItemNotification]) (\s a -> s {notifications = a} :: OpsItem)
{-# DEPRECATED oiNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

instance Lude.FromJSON OpsItem where
  parseJSON =
    Lude.withObject
      "OpsItem"
      ( \x ->
          OpsItem'
            Lude.<$> (x Lude..:? "OpsItemId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Priority")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "Category")
            Lude.<*> (x Lude..:? "Severity")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Source")
            Lude.<*> (x Lude..:? "RelatedOpsItems" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Title")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "OperationalData" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Notifications" Lude..!= Lude.mempty)
      )
