{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
  ( ProvisioningTemplateVersionSummary (..),

    -- * Smart constructor
    mkProvisioningTemplateVersionSummary,

    -- * Lenses
    ptvsVersionId,
    ptvsCreationDate,
    ptvsIsDefaultVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of information about a fleet provision template version.
--
-- /See:/ 'mkProvisioningTemplateVersionSummary' smart constructor.
data ProvisioningTemplateVersionSummary = ProvisioningTemplateVersionSummary'
  { versionId ::
      Lude.Maybe Lude.Int,
    creationDate ::
      Lude.Maybe
        Lude.Timestamp,
    isDefaultVersion ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningTemplateVersionSummary' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date when the fleet provisioning template version was created
-- * 'isDefaultVersion' - True if the fleet provisioning template version is the default version, otherwise false.
-- * 'versionId' - The ID of the fleet privisioning template version.
mkProvisioningTemplateVersionSummary ::
  ProvisioningTemplateVersionSummary
mkProvisioningTemplateVersionSummary =
  ProvisioningTemplateVersionSummary'
    { versionId = Lude.Nothing,
      creationDate = Lude.Nothing,
      isDefaultVersion = Lude.Nothing
    }

-- | The ID of the fleet privisioning template version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptvsVersionId :: Lens.Lens' ProvisioningTemplateVersionSummary (Lude.Maybe Lude.Int)
ptvsVersionId = Lens.lens (versionId :: ProvisioningTemplateVersionSummary -> Lude.Maybe Lude.Int) (\s a -> s {versionId = a} :: ProvisioningTemplateVersionSummary)
{-# DEPRECATED ptvsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The date when the fleet provisioning template version was created
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptvsCreationDate :: Lens.Lens' ProvisioningTemplateVersionSummary (Lude.Maybe Lude.Timestamp)
ptvsCreationDate = Lens.lens (creationDate :: ProvisioningTemplateVersionSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: ProvisioningTemplateVersionSummary)
{-# DEPRECATED ptvsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | True if the fleet provisioning template version is the default version, otherwise false.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptvsIsDefaultVersion :: Lens.Lens' ProvisioningTemplateVersionSummary (Lude.Maybe Lude.Bool)
ptvsIsDefaultVersion = Lens.lens (isDefaultVersion :: ProvisioningTemplateVersionSummary -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: ProvisioningTemplateVersionSummary)
{-# DEPRECATED ptvsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

instance Lude.FromJSON ProvisioningTemplateVersionSummary where
  parseJSON =
    Lude.withObject
      "ProvisioningTemplateVersionSummary"
      ( \x ->
          ProvisioningTemplateVersionSummary'
            Lude.<$> (x Lude..:? "versionId")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "isDefaultVersion")
      )
