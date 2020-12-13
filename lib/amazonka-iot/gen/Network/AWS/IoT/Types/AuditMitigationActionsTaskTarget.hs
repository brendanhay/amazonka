{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsTaskTarget
  ( AuditMitigationActionsTaskTarget (..),

    -- * Smart constructor
    mkAuditMitigationActionsTaskTarget,

    -- * Lenses
    amattAuditTaskId,
    amattFindingIds,
    amattAuditCheckToReasonCodeFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used in MitigationActionParams, this information identifies the target findings to which the mitigation actions are applied. Only one entry appears.
--
-- /See:/ 'mkAuditMitigationActionsTaskTarget' smart constructor.
data AuditMitigationActionsTaskTarget = AuditMitigationActionsTaskTarget'
  { -- | If the task will apply a mitigation action to findings from a specific audit, this value uniquely identifies the audit.
    auditTaskId :: Lude.Maybe Lude.Text,
    -- | If the task will apply a mitigation action to one or more listed findings, this value uniquely identifies those findings.
    findingIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | Specifies a filter in the form of an audit check and set of reason codes that identify the findings from the audit to which the audit mitigation actions task apply.
    auditCheckToReasonCodeFilter :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuditMitigationActionsTaskTarget' with the minimum fields required to make a request.
--
-- * 'auditTaskId' - If the task will apply a mitigation action to findings from a specific audit, this value uniquely identifies the audit.
-- * 'findingIds' - If the task will apply a mitigation action to one or more listed findings, this value uniquely identifies those findings.
-- * 'auditCheckToReasonCodeFilter' - Specifies a filter in the form of an audit check and set of reason codes that identify the findings from the audit to which the audit mitigation actions task apply.
mkAuditMitigationActionsTaskTarget ::
  AuditMitigationActionsTaskTarget
mkAuditMitigationActionsTaskTarget =
  AuditMitigationActionsTaskTarget'
    { auditTaskId = Lude.Nothing,
      findingIds = Lude.Nothing,
      auditCheckToReasonCodeFilter = Lude.Nothing
    }

-- | If the task will apply a mitigation action to findings from a specific audit, this value uniquely identifies the audit.
--
-- /Note:/ Consider using 'auditTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amattAuditTaskId :: Lens.Lens' AuditMitigationActionsTaskTarget (Lude.Maybe Lude.Text)
amattAuditTaskId = Lens.lens (auditTaskId :: AuditMitigationActionsTaskTarget -> Lude.Maybe Lude.Text) (\s a -> s {auditTaskId = a} :: AuditMitigationActionsTaskTarget)
{-# DEPRECATED amattAuditTaskId "Use generic-lens or generic-optics with 'auditTaskId' instead." #-}

-- | If the task will apply a mitigation action to one or more listed findings, this value uniquely identifies those findings.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amattFindingIds :: Lens.Lens' AuditMitigationActionsTaskTarget (Lude.Maybe (Lude.NonEmpty Lude.Text))
amattFindingIds = Lens.lens (findingIds :: AuditMitigationActionsTaskTarget -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {findingIds = a} :: AuditMitigationActionsTaskTarget)
{-# DEPRECATED amattFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}

-- | Specifies a filter in the form of an audit check and set of reason codes that identify the findings from the audit to which the audit mitigation actions task apply.
--
-- /Note:/ Consider using 'auditCheckToReasonCodeFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amattAuditCheckToReasonCodeFilter :: Lens.Lens' AuditMitigationActionsTaskTarget (Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text)))
amattAuditCheckToReasonCodeFilter = Lens.lens (auditCheckToReasonCodeFilter :: AuditMitigationActionsTaskTarget -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.NonEmpty Lude.Text))) (\s a -> s {auditCheckToReasonCodeFilter = a} :: AuditMitigationActionsTaskTarget)
{-# DEPRECATED amattAuditCheckToReasonCodeFilter "Use generic-lens or generic-optics with 'auditCheckToReasonCodeFilter' instead." #-}

instance Lude.FromJSON AuditMitigationActionsTaskTarget where
  parseJSON =
    Lude.withObject
      "AuditMitigationActionsTaskTarget"
      ( \x ->
          AuditMitigationActionsTaskTarget'
            Lude.<$> (x Lude..:? "auditTaskId")
            Lude.<*> (x Lude..:? "findingIds")
            Lude.<*> (x Lude..:? "auditCheckToReasonCodeFilter" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AuditMitigationActionsTaskTarget where
  toJSON AuditMitigationActionsTaskTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("auditTaskId" Lude..=) Lude.<$> auditTaskId,
            ("findingIds" Lude..=) Lude.<$> findingIds,
            ("auditCheckToReasonCodeFilter" Lude..=)
              Lude.<$> auditCheckToReasonCodeFilter
          ]
      )
