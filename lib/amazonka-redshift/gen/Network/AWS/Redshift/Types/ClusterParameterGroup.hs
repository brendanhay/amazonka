{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroup
  ( ClusterParameterGroup (..),

    -- * Smart constructor
    mkClusterParameterGroup,

    -- * Lenses
    cpgParameterGroupFamily,
    cpgDescription,
    cpgTags,
    cpgParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Describes a parameter group.
--
-- /See:/ 'mkClusterParameterGroup' smart constructor.
data ClusterParameterGroup = ClusterParameterGroup'
  { -- | The name of the cluster parameter group family that this cluster parameter group is compatible with.
    parameterGroupFamily :: Lude.Maybe Lude.Text,
    -- | The description of the parameter group.
    description :: Lude.Maybe Lude.Text,
    -- | The list of tags for the cluster parameter group.
    tags :: Lude.Maybe [Tag],
    -- | The name of the cluster parameter group.
    parameterGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterParameterGroup' with the minimum fields required to make a request.
--
-- * 'parameterGroupFamily' - The name of the cluster parameter group family that this cluster parameter group is compatible with.
-- * 'description' - The description of the parameter group.
-- * 'tags' - The list of tags for the cluster parameter group.
-- * 'parameterGroupName' - The name of the cluster parameter group.
mkClusterParameterGroup ::
  ClusterParameterGroup
mkClusterParameterGroup =
  ClusterParameterGroup'
    { parameterGroupFamily = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      parameterGroupName = Lude.Nothing
    }

-- | The name of the cluster parameter group family that this cluster parameter group is compatible with.
--
-- /Note:/ Consider using 'parameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgParameterGroupFamily :: Lens.Lens' ClusterParameterGroup (Lude.Maybe Lude.Text)
cpgParameterGroupFamily = Lens.lens (parameterGroupFamily :: ClusterParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupFamily = a} :: ClusterParameterGroup)
{-# DEPRECATED cpgParameterGroupFamily "Use generic-lens or generic-optics with 'parameterGroupFamily' instead." #-}

-- | The description of the parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgDescription :: Lens.Lens' ClusterParameterGroup (Lude.Maybe Lude.Text)
cpgDescription = Lens.lens (description :: ClusterParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ClusterParameterGroup)
{-# DEPRECATED cpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The list of tags for the cluster parameter group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgTags :: Lens.Lens' ClusterParameterGroup (Lude.Maybe [Tag])
cpgTags = Lens.lens (tags :: ClusterParameterGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ClusterParameterGroup)
{-# DEPRECATED cpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the cluster parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgParameterGroupName :: Lens.Lens' ClusterParameterGroup (Lude.Maybe Lude.Text)
cpgParameterGroupName = Lens.lens (parameterGroupName :: ClusterParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupName = a} :: ClusterParameterGroup)
{-# DEPRECATED cpgParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.FromXML ClusterParameterGroup where
  parseXML x =
    ClusterParameterGroup'
      Lude.<$> (x Lude..@? "ParameterGroupFamily")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
      Lude.<*> (x Lude..@? "ParameterGroupName")
