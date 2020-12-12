{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceItemEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceItemEntry
  ( ComplianceItemEntry (..),

    -- * Smart constructor
    mkComplianceItemEntry,

    -- * Lenses
    cieDetails,
    cieId,
    cieTitle,
    cieSeverity,
    cieStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus

-- | Information about a compliance item.
--
-- /See:/ 'mkComplianceItemEntry' smart constructor.
data ComplianceItemEntry = ComplianceItemEntry'
  { details ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    id :: Lude.Maybe Lude.Text,
    title :: Lude.Maybe Lude.Text,
    severity :: ComplianceSeverity,
    status :: ComplianceStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceItemEntry' with the minimum fields required to make a request.
--
-- * 'details' - A "Key": "Value" tag combination for the compliance item.
-- * 'id' - The compliance item ID. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article.
-- * 'severity' - The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
-- * 'status' - The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
-- * 'title' - The title of the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
mkComplianceItemEntry ::
  -- | 'severity'
  ComplianceSeverity ->
  -- | 'status'
  ComplianceStatus ->
  ComplianceItemEntry
mkComplianceItemEntry pSeverity_ pStatus_ =
  ComplianceItemEntry'
    { details = Lude.Nothing,
      id = Lude.Nothing,
      title = Lude.Nothing,
      severity = pSeverity_,
      status = pStatus_
    }

-- | A "Key": "Value" tag combination for the compliance item.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieDetails :: Lens.Lens' ComplianceItemEntry (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cieDetails = Lens.lens (details :: ComplianceItemEntry -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {details = a} :: ComplianceItemEntry)
{-# DEPRECATED cieDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The compliance item ID. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieId :: Lens.Lens' ComplianceItemEntry (Lude.Maybe Lude.Text)
cieId = Lens.lens (id :: ComplianceItemEntry -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ComplianceItemEntry)
{-# DEPRECATED cieId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The title of the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieTitle :: Lens.Lens' ComplianceItemEntry (Lude.Maybe Lude.Text)
cieTitle = Lens.lens (title :: ComplianceItemEntry -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: ComplianceItemEntry)
{-# DEPRECATED cieTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieSeverity :: Lens.Lens' ComplianceItemEntry ComplianceSeverity
cieSeverity = Lens.lens (severity :: ComplianceItemEntry -> ComplianceSeverity) (\s a -> s {severity = a} :: ComplianceItemEntry)
{-# DEPRECATED cieSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieStatus :: Lens.Lens' ComplianceItemEntry ComplianceStatus
cieStatus = Lens.lens (status :: ComplianceItemEntry -> ComplianceStatus) (\s a -> s {status = a} :: ComplianceItemEntry)
{-# DEPRECATED cieStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.ToJSON ComplianceItemEntry where
  toJSON ComplianceItemEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Details" Lude..=) Lude.<$> details,
            ("Id" Lude..=) Lude.<$> id,
            ("Title" Lude..=) Lude.<$> title,
            Lude.Just ("Severity" Lude..= severity),
            Lude.Just ("Status" Lude..= status)
          ]
      )
