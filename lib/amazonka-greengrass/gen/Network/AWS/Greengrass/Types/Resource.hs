-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Resource
  ( Resource (..),

    -- * Smart constructor
    mkResource,

    -- * Lenses
    rResourceDataContainer,
    rId,
    rName,
  )
where

import Network.AWS.Greengrass.Types.ResourceDataContainer
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a resource.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { resourceDataContainer ::
      ResourceDataContainer,
    id :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- * 'id' - The resource ID, used to refer to a resource in the Lambda function configuration. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
-- * 'name' - The descriptive resource name, which is displayed on the AWS IoT Greengrass console. Max length 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
-- * 'resourceDataContainer' - A container of data for all resource types.
mkResource ::
  -- | 'resourceDataContainer'
  ResourceDataContainer ->
  -- | 'id'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  Resource
mkResource pResourceDataContainer_ pId_ pName_ =
  Resource'
    { resourceDataContainer = pResourceDataContainer_,
      id = pId_,
      name = pName_
    }

-- | A container of data for all resource types.
--
-- /Note:/ Consider using 'resourceDataContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResourceDataContainer :: Lens.Lens' Resource ResourceDataContainer
rResourceDataContainer = Lens.lens (resourceDataContainer :: Resource -> ResourceDataContainer) (\s a -> s {resourceDataContainer = a} :: Resource)
{-# DEPRECATED rResourceDataContainer "Use generic-lens or generic-optics with 'resourceDataContainer' instead." #-}

-- | The resource ID, used to refer to a resource in the Lambda function configuration. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' Resource Lude.Text
rId = Lens.lens (id :: Resource -> Lude.Text) (\s a -> s {id = a} :: Resource)
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The descriptive resource name, which is displayed on the AWS IoT Greengrass console. Max length 128 characters with pattern ''[a-zA-Z0-9:_-]+''. This must be unique within a Greengrass group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Resource Lude.Text
rName = Lens.lens (name :: Resource -> Lude.Text) (\s a -> s {name = a} :: Resource)
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Resource where
  parseJSON =
    Lude.withObject
      "Resource"
      ( \x ->
          Resource'
            Lude.<$> (x Lude..: "ResourceDataContainer")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON Resource where
  toJSON Resource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceDataContainer" Lude..= resourceDataContainer),
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("Name" Lude..= name)
          ]
      )
