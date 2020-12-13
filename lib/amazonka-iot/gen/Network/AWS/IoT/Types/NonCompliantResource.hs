{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.NonCompliantResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.NonCompliantResource
  ( NonCompliantResource (..),

    -- * Smart constructor
    mkNonCompliantResource,

    -- * Lenses
    ncrAdditionalInfo,
    ncrResourceType,
    ncrResourceIdentifier,
  )
where

import Network.AWS.IoT.Types.ResourceIdentifier
import Network.AWS.IoT.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the resource that was noncompliant with the audit check.
--
-- /See:/ 'mkNonCompliantResource' smart constructor.
data NonCompliantResource = NonCompliantResource'
  { -- | Other information about the noncompliant resource.
    additionalInfo :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The type of the noncompliant resource.
    resourceType :: Lude.Maybe ResourceType,
    -- | Information that identifies the noncompliant resource.
    resourceIdentifier :: Lude.Maybe ResourceIdentifier
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NonCompliantResource' with the minimum fields required to make a request.
--
-- * 'additionalInfo' - Other information about the noncompliant resource.
-- * 'resourceType' - The type of the noncompliant resource.
-- * 'resourceIdentifier' - Information that identifies the noncompliant resource.
mkNonCompliantResource ::
  NonCompliantResource
mkNonCompliantResource =
  NonCompliantResource'
    { additionalInfo = Lude.Nothing,
      resourceType = Lude.Nothing,
      resourceIdentifier = Lude.Nothing
    }

-- | Other information about the noncompliant resource.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncrAdditionalInfo :: Lens.Lens' NonCompliantResource (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ncrAdditionalInfo = Lens.lens (additionalInfo :: NonCompliantResource -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {additionalInfo = a} :: NonCompliantResource)
{-# DEPRECATED ncrAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | The type of the noncompliant resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncrResourceType :: Lens.Lens' NonCompliantResource (Lude.Maybe ResourceType)
ncrResourceType = Lens.lens (resourceType :: NonCompliantResource -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: NonCompliantResource)
{-# DEPRECATED ncrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | Information that identifies the noncompliant resource.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncrResourceIdentifier :: Lens.Lens' NonCompliantResource (Lude.Maybe ResourceIdentifier)
ncrResourceIdentifier = Lens.lens (resourceIdentifier :: NonCompliantResource -> Lude.Maybe ResourceIdentifier) (\s a -> s {resourceIdentifier = a} :: NonCompliantResource)
{-# DEPRECATED ncrResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Lude.FromJSON NonCompliantResource where
  parseJSON =
    Lude.withObject
      "NonCompliantResource"
      ( \x ->
          NonCompliantResource'
            Lude.<$> (x Lude..:? "additionalInfo" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "resourceIdentifier")
      )
