-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemSummary
  ( OpsItemSummary (..),

    -- * Smart constructor
    mkOpsItemSummary,

    -- * Lenses
    oisOpsItemId,
    oisStatus,
    oisPriority,
    oisCreatedTime,
    oisCategory,
    oisSeverity,
    oisCreatedBy,
    oisLastModifiedTime,
    oisSource,
    oisTitle,
    oisLastModifiedBy,
    oisOperationalData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.OpsItemDataValue
import Network.AWS.SSM.Types.OpsItemStatus

-- | A count of OpsItems.
--
-- /See:/ 'mkOpsItemSummary' smart constructor.
data OpsItemSummary = OpsItemSummary'
  { opsItemId ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe OpsItemStatus,
    priority :: Lude.Maybe Lude.Natural,
    createdTime :: Lude.Maybe Lude.Timestamp,
    category :: Lude.Maybe Lude.Text,
    severity :: Lude.Maybe Lude.Text,
    createdBy :: Lude.Maybe Lude.Text,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    source :: Lude.Maybe Lude.Text,
    title :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe Lude.Text,
    operationalData ::
      Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsItemSummary' with the minimum fields required to make a request.
--
-- * 'category' - A list of OpsItems by category.
-- * 'createdBy' - The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
-- * 'createdTime' - The date and time the OpsItem was created.
-- * 'lastModifiedBy' - The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
-- * 'lastModifiedTime' - The date and time the OpsItem was last updated.
-- * 'operationalData' - Operational data is custom data that provides useful reference details about the OpsItem.
-- * 'opsItemId' - The ID of the OpsItem.
-- * 'priority' - The importance of this OpsItem in relation to other OpsItems in the system.
-- * 'severity' - A list of OpsItems by severity.
-- * 'source' - The impacted AWS resource.
-- * 'status' - The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ .
-- * 'title' - A short heading that describes the nature of the OpsItem and the impacted resource.
mkOpsItemSummary ::
  OpsItemSummary
mkOpsItemSummary =
  OpsItemSummary'
    { opsItemId = Lude.Nothing,
      status = Lude.Nothing,
      priority = Lude.Nothing,
      createdTime = Lude.Nothing,
      category = Lude.Nothing,
      severity = Lude.Nothing,
      createdBy = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      source = Lude.Nothing,
      title = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      operationalData = Lude.Nothing
    }

-- | The ID of the OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisOpsItemId :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Text)
oisOpsItemId = Lens.lens (opsItemId :: OpsItemSummary -> Lude.Maybe Lude.Text) (\s a -> s {opsItemId = a} :: OpsItemSummary)
{-# DEPRECATED oisOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

-- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisStatus :: Lens.Lens' OpsItemSummary (Lude.Maybe OpsItemStatus)
oisStatus = Lens.lens (status :: OpsItemSummary -> Lude.Maybe OpsItemStatus) (\s a -> s {status = a} :: OpsItemSummary)
{-# DEPRECATED oisStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The importance of this OpsItem in relation to other OpsItems in the system.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisPriority :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Natural)
oisPriority = Lens.lens (priority :: OpsItemSummary -> Lude.Maybe Lude.Natural) (\s a -> s {priority = a} :: OpsItemSummary)
{-# DEPRECATED oisPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The date and time the OpsItem was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisCreatedTime :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Timestamp)
oisCreatedTime = Lens.lens (createdTime :: OpsItemSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: OpsItemSummary)
{-# DEPRECATED oisCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | A list of OpsItems by category.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisCategory :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Text)
oisCategory = Lens.lens (category :: OpsItemSummary -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: OpsItemSummary)
{-# DEPRECATED oisCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | A list of OpsItems by severity.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisSeverity :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Text)
oisSeverity = Lens.lens (severity :: OpsItemSummary -> Lude.Maybe Lude.Text) (\s a -> s {severity = a} :: OpsItemSummary)
{-# DEPRECATED oisSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisCreatedBy :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Text)
oisCreatedBy = Lens.lens (createdBy :: OpsItemSummary -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: OpsItemSummary)
{-# DEPRECATED oisCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The date and time the OpsItem was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisLastModifiedTime :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Timestamp)
oisLastModifiedTime = Lens.lens (lastModifiedTime :: OpsItemSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: OpsItemSummary)
{-# DEPRECATED oisLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The impacted AWS resource.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisSource :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Text)
oisSource = Lens.lens (source :: OpsItemSummary -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: OpsItemSummary)
{-# DEPRECATED oisSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisTitle :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Text)
oisTitle = Lens.lens (title :: OpsItemSummary -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: OpsItemSummary)
{-# DEPRECATED oisTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisLastModifiedBy :: Lens.Lens' OpsItemSummary (Lude.Maybe Lude.Text)
oisLastModifiedBy = Lens.lens (lastModifiedBy :: OpsItemSummary -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: OpsItemSummary)
{-# DEPRECATED oisLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | Operational data is custom data that provides useful reference details about the OpsItem.
--
-- /Note:/ Consider using 'operationalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisOperationalData :: Lens.Lens' OpsItemSummary (Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue)))
oisOperationalData = Lens.lens (operationalData :: OpsItemSummary -> Lude.Maybe (Lude.HashMap Lude.Text (OpsItemDataValue))) (\s a -> s {operationalData = a} :: OpsItemSummary)
{-# DEPRECATED oisOperationalData "Use generic-lens or generic-optics with 'operationalData' instead." #-}

instance Lude.FromJSON OpsItemSummary where
  parseJSON =
    Lude.withObject
      "OpsItemSummary"
      ( \x ->
          OpsItemSummary'
            Lude.<$> (x Lude..:? "OpsItemId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Priority")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "Category")
            Lude.<*> (x Lude..:? "Severity")
            Lude.<*> (x Lude..:? "CreatedBy")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "Source")
            Lude.<*> (x Lude..:? "Title")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "OperationalData" Lude..!= Lude.mempty)
      )
