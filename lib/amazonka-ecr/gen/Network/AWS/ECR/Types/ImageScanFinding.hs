{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanFinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanFinding
  ( ImageScanFinding (..),

    -- * Smart constructor
    mkImageScanFinding,

    -- * Lenses
    isfSeverity,
    isfUri,
    isfName,
    isfAttributes,
    isfDescription,
  )
where

import Network.AWS.ECR.Types.Attribute
import Network.AWS.ECR.Types.FindingSeverity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an image scan finding.
--
-- /See:/ 'mkImageScanFinding' smart constructor.
data ImageScanFinding = ImageScanFinding'
  { -- | The finding severity.
    severity :: Lude.Maybe FindingSeverity,
    -- | A link containing additional details about the security vulnerability.
    uri :: Lude.Maybe Lude.Text,
    -- | The name associated with the finding, usually a CVE number.
    name :: Lude.Maybe Lude.Text,
    -- | A collection of attributes of the host from which the finding is generated.
    attributes :: Lude.Maybe [Attribute],
    -- | The description of the finding.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageScanFinding' with the minimum fields required to make a request.
--
-- * 'severity' - The finding severity.
-- * 'uri' - A link containing additional details about the security vulnerability.
-- * 'name' - The name associated with the finding, usually a CVE number.
-- * 'attributes' - A collection of attributes of the host from which the finding is generated.
-- * 'description' - The description of the finding.
mkImageScanFinding ::
  ImageScanFinding
mkImageScanFinding =
  ImageScanFinding'
    { severity = Lude.Nothing,
      uri = Lude.Nothing,
      name = Lude.Nothing,
      attributes = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The finding severity.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfSeverity :: Lens.Lens' ImageScanFinding (Lude.Maybe FindingSeverity)
isfSeverity = Lens.lens (severity :: ImageScanFinding -> Lude.Maybe FindingSeverity) (\s a -> s {severity = a} :: ImageScanFinding)
{-# DEPRECATED isfSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | A link containing additional details about the security vulnerability.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfUri :: Lens.Lens' ImageScanFinding (Lude.Maybe Lude.Text)
isfUri = Lens.lens (uri :: ImageScanFinding -> Lude.Maybe Lude.Text) (\s a -> s {uri = a} :: ImageScanFinding)
{-# DEPRECATED isfUri "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | The name associated with the finding, usually a CVE number.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfName :: Lens.Lens' ImageScanFinding (Lude.Maybe Lude.Text)
isfName = Lens.lens (name :: ImageScanFinding -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ImageScanFinding)
{-# DEPRECATED isfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A collection of attributes of the host from which the finding is generated.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfAttributes :: Lens.Lens' ImageScanFinding (Lude.Maybe [Attribute])
isfAttributes = Lens.lens (attributes :: ImageScanFinding -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: ImageScanFinding)
{-# DEPRECATED isfAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The description of the finding.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfDescription :: Lens.Lens' ImageScanFinding (Lude.Maybe Lude.Text)
isfDescription = Lens.lens (description :: ImageScanFinding -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImageScanFinding)
{-# DEPRECATED isfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ImageScanFinding where
  parseJSON =
    Lude.withObject
      "ImageScanFinding"
      ( \x ->
          ImageScanFinding'
            Lude.<$> (x Lude..:? "severity")
            Lude.<*> (x Lude..:? "uri")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "description")
      )
