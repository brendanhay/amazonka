{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExceptionResourceKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExceptionResourceKey
  ( RemediationExceptionResourceKey (..),

    -- * Smart constructor
    mkRemediationExceptionResourceKey,

    -- * Lenses
    rerkResourceId,
    rerkResourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details that identify a resource within AWS Config, including the resource type and resource ID.
--
-- /See:/ 'mkRemediationExceptionResourceKey' smart constructor.
data RemediationExceptionResourceKey = RemediationExceptionResourceKey'
  { resourceId ::
      Lude.Maybe Lude.Text,
    resourceType ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemediationExceptionResourceKey' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource (for example., sg-xxxxxx).
-- * 'resourceType' - The type of a resource.
mkRemediationExceptionResourceKey ::
  RemediationExceptionResourceKey
mkRemediationExceptionResourceKey =
  RemediationExceptionResourceKey'
    { resourceId = Lude.Nothing,
      resourceType = Lude.Nothing
    }

-- | The ID of the resource (for example., sg-xxxxxx).
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerkResourceId :: Lens.Lens' RemediationExceptionResourceKey (Lude.Maybe Lude.Text)
rerkResourceId = Lens.lens (resourceId :: RemediationExceptionResourceKey -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: RemediationExceptionResourceKey)
{-# DEPRECATED rerkResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of a resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rerkResourceType :: Lens.Lens' RemediationExceptionResourceKey (Lude.Maybe Lude.Text)
rerkResourceType = Lens.lens (resourceType :: RemediationExceptionResourceKey -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: RemediationExceptionResourceKey)
{-# DEPRECATED rerkResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Lude.FromJSON RemediationExceptionResourceKey where
  parseJSON =
    Lude.withObject
      "RemediationExceptionResourceKey"
      ( \x ->
          RemediationExceptionResourceKey'
            Lude.<$> (x Lude..:? "ResourceId") Lude.<*> (x Lude..:? "ResourceType")
      )

instance Lude.ToJSON RemediationExceptionResourceKey where
  toJSON RemediationExceptionResourceKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceId" Lude..=) Lude.<$> resourceId,
            ("ResourceType" Lude..=) Lude.<$> resourceType
          ]
      )
