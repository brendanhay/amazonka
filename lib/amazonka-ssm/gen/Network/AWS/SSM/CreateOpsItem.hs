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
    coiPriority,
    coiCategory,
    coiSeverity,
    coiSource,
    coiRelatedOpsItems,
    coiTitle,
    coiOperationalData,
    coiDescription,
    coiNotifications,
    coiTags,

    -- * Destructuring the response
    CreateOpsItemResponse (..),
    mkCreateOpsItemResponse,

    -- ** Response lenses
    coirsOpsItemId,
    coirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkCreateOpsItem' smart constructor.
data CreateOpsItem = CreateOpsItem'
  { -- | The importance of this OpsItem in relation to other OpsItems in the system.
    priority :: Lude.Maybe Lude.Natural,
    -- | Specify a category to assign to an OpsItem.
    category :: Lude.Maybe Lude.Text,
    -- | Specify a severity to assign to an OpsItem.
    severity :: Lude.Maybe Lude.Text,
    -- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
    source :: Lude.Text,
    -- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
    relatedOpsItems :: Lude.Maybe [RelatedOpsItem],
    -- | A short heading that describes the nature of the OpsItem and the impacted resource.
    title :: Lude.Text,
    -- | Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
    --
    -- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
    -- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
    -- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
    operationalData :: Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue)),
    -- | Information about the OpsItem.
    description :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
    notifications :: Lude.Maybe [OpsItemNotification],
    -- | Optional metadata that you assign to a resource. You can restrict access to OpsItems by using an inline IAM policy that specifies tags. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
    --
    -- Tags use a key-value pair. For example:
    -- @Key=Department,Value=Finance@
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOpsItem' with the minimum fields required to make a request.
--
-- * 'priority' - The importance of this OpsItem in relation to other OpsItems in the system.
-- * 'category' - Specify a category to assign to an OpsItem.
-- * 'severity' - Specify a severity to assign to an OpsItem.
-- * 'source' - The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
-- * 'relatedOpsItems' - One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
-- * 'title' - A short heading that describes the nature of the OpsItem and the impacted resource.
-- * 'operationalData' - Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
--
-- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
-- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
-- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
-- * 'description' - Information about the OpsItem.
-- * 'notifications' - The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
-- * 'tags' - Optional metadata that you assign to a resource. You can restrict access to OpsItems by using an inline IAM policy that specifies tags. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- Tags use a key-value pair. For example:
-- @Key=Department,Value=Finance@
mkCreateOpsItem ::
  -- | 'source'
  Lude.Text ->
  -- | 'title'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  CreateOpsItem
mkCreateOpsItem pSource_ pTitle_ pDescription_ =
  CreateOpsItem'
    { priority = Lude.Nothing,
      category = Lude.Nothing,
      severity = Lude.Nothing,
      source = pSource_,
      relatedOpsItems = Lude.Nothing,
      title = pTitle_,
      operationalData = Lude.Nothing,
      description = pDescription_,
      notifications = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The importance of this OpsItem in relation to other OpsItems in the system.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiPriority :: Lens.Lens' CreateOpsItem (Lude.Maybe Lude.Natural)
coiPriority = Lens.lens (priority :: CreateOpsItem -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: CreateOpsItem)
{-# DEPRECATED coiPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Specify a category to assign to an OpsItem.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiCategory :: Lens.Lens' CreateOpsItem (Lude.Maybe Lude.Text)
coiCategory = Lens.lens (category :: CreateOpsItem -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: CreateOpsItem)
{-# DEPRECATED coiCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Specify a severity to assign to an OpsItem.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiSeverity :: Lens.Lens' CreateOpsItem (Lude.Maybe Lude.Text)
coiSeverity = Lens.lens (severity :: CreateOpsItem -> Lude.Maybe Lude.Text) (\s a -> s {severity = a} :: CreateOpsItem)
{-# DEPRECATED coiSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The origin of the OpsItem, such as Amazon EC2 or Systems Manager.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiSource :: Lens.Lens' CreateOpsItem Lude.Text
coiSource = Lens.lens (source :: CreateOpsItem -> Lude.Text) (\s a -> s {source = a} :: CreateOpsItem)
{-# DEPRECATED coiSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | One or more OpsItems that share something in common with the current OpsItems. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- /Note:/ Consider using 'relatedOpsItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiRelatedOpsItems :: Lens.Lens' CreateOpsItem (Lude.Maybe [RelatedOpsItem])
coiRelatedOpsItems = Lens.lens (relatedOpsItems :: CreateOpsItem -> Lude.Maybe [RelatedOpsItem]) (\s a -> s {relatedOpsItems = a} :: CreateOpsItem)
{-# DEPRECATED coiRelatedOpsItems "Use generic-lens or generic-optics with 'relatedOpsItems' instead." #-}

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiTitle :: Lens.Lens' CreateOpsItem Lude.Text
coiTitle = Lens.lens (title :: CreateOpsItem -> Lude.Text) (\s a -> s {title = a} :: CreateOpsItem)
{-# DEPRECATED coiTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | Operational data is custom data that provides useful reference details about the OpsItem. For example, you can specify log files, error strings, license keys, troubleshooting tips, or other relevant data. You enter operational data as key-value pairs. The key has a maximum length of 128 characters. The value has a maximum size of 20 KB.
--
-- /Important:/ Operational data keys /can't/ begin with the following: amazon, aws, amzn, ssm, /amazon, /aws, /amzn, /ssm.
-- You can choose to make the data searchable by other users in the account or you can restrict search access. Searchable data means that all users with access to the OpsItem Overview page (as provided by the 'DescribeOpsItems' API action) can view and search on the specified data. Operational data that is not searchable is only viewable by users who have access to the OpsItem (as provided by the 'GetOpsItem' API action).
-- Use the @/aws/resources@ key in OperationalData to specify a related resource in the request. Use the @/aws/automations@ key in OperationalData to associate an Automation runbook with the OpsItem. To view AWS CLI example commands that use these keys, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-creating-OpsItems.html#OpsCenter-manually-create-OpsItems Creating OpsItems manually> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'operationalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiOperationalData :: Lens.Lens' CreateOpsItem (Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue)))
coiOperationalData = Lens.lens (operationalData :: CreateOpsItem -> Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue))) (\s a -> s {operationalData = a} :: CreateOpsItem)
{-# DEPRECATED coiOperationalData "Use generic-lens or generic-optics with 'operationalData' instead." #-}

-- | Information about the OpsItem.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiDescription :: Lens.Lens' CreateOpsItem Lude.Text
coiDescription = Lens.lens (description :: CreateOpsItem -> Lude.Text) (\s a -> s {description = a} :: CreateOpsItem)
{-# DEPRECATED coiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The Amazon Resource Name (ARN) of an SNS topic where notifications are sent when this OpsItem is edited or changed.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiNotifications :: Lens.Lens' CreateOpsItem (Lude.Maybe [OpsItemNotification])
coiNotifications = Lens.lens (notifications :: CreateOpsItem -> Lude.Maybe [OpsItemNotification]) (\s a -> s {notifications = a} :: CreateOpsItem)
{-# DEPRECATED coiNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

-- | Optional metadata that you assign to a resource. You can restrict access to OpsItems by using an inline IAM policy that specifies tags. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html#OpsCenter-getting-started-user-permissions Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- Tags use a key-value pair. For example:
-- @Key=Department,Value=Finance@
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coiTags :: Lens.Lens' CreateOpsItem (Lude.Maybe [Tag])
coiTags = Lens.lens (tags :: CreateOpsItem -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateOpsItem)
{-# DEPRECATED coiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateOpsItem where
  type Rs CreateOpsItem = CreateOpsItemResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateOpsItemResponse'
            Lude.<$> (x Lude..?> "OpsItemId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateOpsItem where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CreateOpsItem" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateOpsItem where
  toJSON CreateOpsItem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Priority" Lude..=) Lude.<$> priority,
            ("Category" Lude..=) Lude.<$> category,
            ("Severity" Lude..=) Lude.<$> severity,
            Lude.Just ("Source" Lude..= source),
            ("RelatedOpsItems" Lude..=) Lude.<$> relatedOpsItems,
            Lude.Just ("Title" Lude..= title),
            ("OperationalData" Lude..=) Lude.<$> operationalData,
            Lude.Just ("Description" Lude..= description),
            ("Notifications" Lude..=) Lude.<$> notifications,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateOpsItem where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateOpsItem where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateOpsItemResponse' smart constructor.
data CreateOpsItemResponse = CreateOpsItemResponse'
  { -- | The ID of the OpsItem.
    opsItemId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateOpsItemResponse' with the minimum fields required to make a request.
--
-- * 'opsItemId' - The ID of the OpsItem.
-- * 'responseStatus' - The response status code.
mkCreateOpsItemResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateOpsItemResponse
mkCreateOpsItemResponse pResponseStatus_ =
  CreateOpsItemResponse'
    { opsItemId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coirsOpsItemId :: Lens.Lens' CreateOpsItemResponse (Lude.Maybe Lude.Text)
coirsOpsItemId = Lens.lens (opsItemId :: CreateOpsItemResponse -> Lude.Maybe Lude.Text) (\s a -> s {opsItemId = a} :: CreateOpsItemResponse)
{-# DEPRECATED coirsOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coirsResponseStatus :: Lens.Lens' CreateOpsItemResponse Lude.Int
coirsResponseStatus = Lens.lens (responseStatus :: CreateOpsItemResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateOpsItemResponse)
{-# DEPRECATED coirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
