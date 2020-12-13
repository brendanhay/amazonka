{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceItem
  ( ComplianceItem (..),

    -- * Smart constructor
    mkComplianceItem,

    -- * Lenses
    ciStatus,
    ciResourceId,
    ciResourceType,
    ciSeverity,
    ciExecutionSummary,
    ciDetails,
    ciId,
    ciComplianceType,
    ciTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus

-- | Information about the compliance as defined by the resource type. For example, for a patch resource type, @Items@ includes information about the PatchSeverity, Classification, and so on.
--
-- /See:/ 'mkComplianceItem' smart constructor.
data ComplianceItem = ComplianceItem'
  { -- | The status of the compliance item. An item is either COMPLIANT, NON_COMPLIANT, or an empty string (for Windows patches that aren't applicable).
    status :: Lude.Maybe ComplianceStatus,
    -- | An ID for the resource. For a managed instance, this is the instance ID.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The type of resource. @ManagedInstance@ is currently the only supported resource type.
    resourceType :: Lude.Maybe Lude.Text,
    -- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
    severity :: Lude.Maybe ComplianceSeverity,
    -- | A summary for the compliance item. The summary includes an execution ID, the execution type (for example, command), and the execution time.
    executionSummary :: Lude.Maybe ComplianceExecutionSummary,
    -- | A "Key": "Value" tag combination for the compliance item.
    details :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | An ID for the compliance item. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article; for example: KB4010320.
    id :: Lude.Maybe Lude.Text,
    -- | The compliance type. For example, Association (for a State Manager association), Patch, or Custom:@string@ are all valid compliance types.
    complianceType :: Lude.Maybe Lude.Text,
    -- | A title for the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
    title :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceItem' with the minimum fields required to make a request.
--
-- * 'status' - The status of the compliance item. An item is either COMPLIANT, NON_COMPLIANT, or an empty string (for Windows patches that aren't applicable).
-- * 'resourceId' - An ID for the resource. For a managed instance, this is the instance ID.
-- * 'resourceType' - The type of resource. @ManagedInstance@ is currently the only supported resource type.
-- * 'severity' - The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
-- * 'executionSummary' - A summary for the compliance item. The summary includes an execution ID, the execution type (for example, command), and the execution time.
-- * 'details' - A "Key": "Value" tag combination for the compliance item.
-- * 'id' - An ID for the compliance item. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article; for example: KB4010320.
-- * 'complianceType' - The compliance type. For example, Association (for a State Manager association), Patch, or Custom:@string@ are all valid compliance types.
-- * 'title' - A title for the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
mkComplianceItem ::
  ComplianceItem
mkComplianceItem =
  ComplianceItem'
    { status = Lude.Nothing,
      resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      severity = Lude.Nothing,
      executionSummary = Lude.Nothing,
      details = Lude.Nothing,
      id = Lude.Nothing,
      complianceType = Lude.Nothing,
      title = Lude.Nothing
    }

-- | The status of the compliance item. An item is either COMPLIANT, NON_COMPLIANT, or an empty string (for Windows patches that aren't applicable).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciStatus :: Lens.Lens' ComplianceItem (Lude.Maybe ComplianceStatus)
ciStatus = Lens.lens (status :: ComplianceItem -> Lude.Maybe ComplianceStatus) (\s a -> s {status = a} :: ComplianceItem)
{-# DEPRECATED ciStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An ID for the resource. For a managed instance, this is the instance ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceId :: Lens.Lens' ComplianceItem (Lude.Maybe Lude.Text)
ciResourceId = Lens.lens (resourceId :: ComplianceItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ComplianceItem)
{-# DEPRECATED ciResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource. @ManagedInstance@ is currently the only supported resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciResourceType :: Lens.Lens' ComplianceItem (Lude.Maybe Lude.Text)
ciResourceType = Lens.lens (resourceType :: ComplianceItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: ComplianceItem)
{-# DEPRECATED ciResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSeverity :: Lens.Lens' ComplianceItem (Lude.Maybe ComplianceSeverity)
ciSeverity = Lens.lens (severity :: ComplianceItem -> Lude.Maybe ComplianceSeverity) (\s a -> s {severity = a} :: ComplianceItem)
{-# DEPRECATED ciSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | A summary for the compliance item. The summary includes an execution ID, the execution type (for example, command), and the execution time.
--
-- /Note:/ Consider using 'executionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciExecutionSummary :: Lens.Lens' ComplianceItem (Lude.Maybe ComplianceExecutionSummary)
ciExecutionSummary = Lens.lens (executionSummary :: ComplianceItem -> Lude.Maybe ComplianceExecutionSummary) (\s a -> s {executionSummary = a} :: ComplianceItem)
{-# DEPRECATED ciExecutionSummary "Use generic-lens or generic-optics with 'executionSummary' instead." #-}

-- | A "Key": "Value" tag combination for the compliance item.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDetails :: Lens.Lens' ComplianceItem (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ciDetails = Lens.lens (details :: ComplianceItem -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {details = a} :: ComplianceItem)
{-# DEPRECATED ciDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | An ID for the compliance item. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article; for example: KB4010320.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciId :: Lens.Lens' ComplianceItem (Lude.Maybe Lude.Text)
ciId = Lens.lens (id :: ComplianceItem -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ComplianceItem)
{-# DEPRECATED ciId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The compliance type. For example, Association (for a State Manager association), Patch, or Custom:@string@ are all valid compliance types.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciComplianceType :: Lens.Lens' ComplianceItem (Lude.Maybe Lude.Text)
ciComplianceType = Lens.lens (complianceType :: ComplianceItem -> Lude.Maybe Lude.Text) (\s a -> s {complianceType = a} :: ComplianceItem)
{-# DEPRECATED ciComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | A title for the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTitle :: Lens.Lens' ComplianceItem (Lude.Maybe Lude.Text)
ciTitle = Lens.lens (title :: ComplianceItem -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: ComplianceItem)
{-# DEPRECATED ciTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.FromJSON ComplianceItem where
  parseJSON =
    Lude.withObject
      "ComplianceItem"
      ( \x ->
          ComplianceItem'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "Severity")
            Lude.<*> (x Lude..:? "ExecutionSummary")
            Lude.<*> (x Lude..:? "Details" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "ComplianceType")
            Lude.<*> (x Lude..:? "Title")
      )
