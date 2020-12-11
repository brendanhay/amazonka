-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ProvisioningTemplateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningTemplateSummary
  ( ProvisioningTemplateSummary (..),

    -- * Smart constructor
    mkProvisioningTemplateSummary,

    -- * Lenses
    ptsLastModifiedDate,
    ptsTemplateName,
    ptsEnabled,
    ptsCreationDate,
    ptsTemplateARN,
    ptsDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of information about a fleet provisioning template.
--
-- /See:/ 'mkProvisioningTemplateSummary' smart constructor.
data ProvisioningTemplateSummary = ProvisioningTemplateSummary'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    templateName ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    creationDate ::
      Lude.Maybe Lude.Timestamp,
    templateARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningTemplateSummary' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date when the fleet provisioning template summary was created.
-- * 'description' - The description of the fleet provisioning template.
-- * 'enabled' - True if the fleet provision template is enabled, otherwise false.
-- * 'lastModifiedDate' - The date when the fleet provisioning template summary was last modified.
-- * 'templateARN' - The ARN of the fleet provisioning template.
-- * 'templateName' - The name of the fleet provisioning template.
mkProvisioningTemplateSummary ::
  ProvisioningTemplateSummary
mkProvisioningTemplateSummary =
  ProvisioningTemplateSummary'
    { lastModifiedDate = Lude.Nothing,
      templateName = Lude.Nothing,
      enabled = Lude.Nothing,
      creationDate = Lude.Nothing,
      templateARN = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The date when the fleet provisioning template summary was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsLastModifiedDate :: Lens.Lens' ProvisioningTemplateSummary (Lude.Maybe Lude.Timestamp)
ptsLastModifiedDate = Lens.lens (lastModifiedDate :: ProvisioningTemplateSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: ProvisioningTemplateSummary)
{-# DEPRECATED ptsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsTemplateName :: Lens.Lens' ProvisioningTemplateSummary (Lude.Maybe Lude.Text)
ptsTemplateName = Lens.lens (templateName :: ProvisioningTemplateSummary -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: ProvisioningTemplateSummary)
{-# DEPRECATED ptsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | True if the fleet provision template is enabled, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsEnabled :: Lens.Lens' ProvisioningTemplateSummary (Lude.Maybe Lude.Bool)
ptsEnabled = Lens.lens (enabled :: ProvisioningTemplateSummary -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ProvisioningTemplateSummary)
{-# DEPRECATED ptsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The date when the fleet provisioning template summary was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsCreationDate :: Lens.Lens' ProvisioningTemplateSummary (Lude.Maybe Lude.Timestamp)
ptsCreationDate = Lens.lens (creationDate :: ProvisioningTemplateSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: ProvisioningTemplateSummary)
{-# DEPRECATED ptsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The ARN of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsTemplateARN :: Lens.Lens' ProvisioningTemplateSummary (Lude.Maybe Lude.Text)
ptsTemplateARN = Lens.lens (templateARN :: ProvisioningTemplateSummary -> Lude.Maybe Lude.Text) (\s a -> s {templateARN = a} :: ProvisioningTemplateSummary)
{-# DEPRECATED ptsTemplateARN "Use generic-lens or generic-optics with 'templateARN' instead." #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsDescription :: Lens.Lens' ProvisioningTemplateSummary (Lude.Maybe Lude.Text)
ptsDescription = Lens.lens (description :: ProvisioningTemplateSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ProvisioningTemplateSummary)
{-# DEPRECATED ptsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ProvisioningTemplateSummary where
  parseJSON =
    Lude.withObject
      "ProvisioningTemplateSummary"
      ( \x ->
          ProvisioningTemplateSummary'
            Lude.<$> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "templateName")
            Lude.<*> (x Lude..:? "enabled")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "templateArn")
            Lude.<*> (x Lude..:? "description")
      )
