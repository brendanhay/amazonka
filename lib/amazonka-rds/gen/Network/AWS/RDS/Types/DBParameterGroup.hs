{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBParameterGroup
  ( DBParameterGroup (..),

    -- * Smart constructor
    mkDBParameterGroup,

    -- * Lenses
    dpgDBParameterGroupARN,
    dpgDBParameterGroupFamily,
    dpgDBParameterGroupName,
    dpgDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the details of an Amazon RDS DB parameter group.
--
-- This data type is used as a response element in the @DescribeDBParameterGroups@ action.
--
-- /See:/ 'mkDBParameterGroup' smart constructor.
data DBParameterGroup = DBParameterGroup'
  { -- | The Amazon Resource Name (ARN) for the DB parameter group.
    dbParameterGroupARN :: Lude.Maybe Lude.Text,
    -- | The name of the DB parameter group family that this DB parameter group is compatible with.
    dbParameterGroupFamily :: Lude.Maybe Lude.Text,
    -- | The name of the DB parameter group.
    dbParameterGroupName :: Lude.Maybe Lude.Text,
    -- | Provides the customer-specified description for this DB parameter group.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBParameterGroup' with the minimum fields required to make a request.
--
-- * 'dbParameterGroupARN' - The Amazon Resource Name (ARN) for the DB parameter group.
-- * 'dbParameterGroupFamily' - The name of the DB parameter group family that this DB parameter group is compatible with.
-- * 'dbParameterGroupName' - The name of the DB parameter group.
-- * 'description' - Provides the customer-specified description for this DB parameter group.
mkDBParameterGroup ::
  DBParameterGroup
mkDBParameterGroup =
  DBParameterGroup'
    { dbParameterGroupARN = Lude.Nothing,
      dbParameterGroupFamily = Lude.Nothing,
      dbParameterGroupName = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the DB parameter group.
--
-- /Note:/ Consider using 'dbParameterGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgDBParameterGroupARN :: Lens.Lens' DBParameterGroup (Lude.Maybe Lude.Text)
dpgDBParameterGroupARN = Lens.lens (dbParameterGroupARN :: DBParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupARN = a} :: DBParameterGroup)
{-# DEPRECATED dpgDBParameterGroupARN "Use generic-lens or generic-optics with 'dbParameterGroupARN' instead." #-}

-- | The name of the DB parameter group family that this DB parameter group is compatible with.
--
-- /Note:/ Consider using 'dbParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgDBParameterGroupFamily :: Lens.Lens' DBParameterGroup (Lude.Maybe Lude.Text)
dpgDBParameterGroupFamily = Lens.lens (dbParameterGroupFamily :: DBParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupFamily = a} :: DBParameterGroup)
{-# DEPRECATED dpgDBParameterGroupFamily "Use generic-lens or generic-optics with 'dbParameterGroupFamily' instead." #-}

-- | The name of the DB parameter group.
--
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgDBParameterGroupName :: Lens.Lens' DBParameterGroup (Lude.Maybe Lude.Text)
dpgDBParameterGroupName = Lens.lens (dbParameterGroupName :: DBParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: DBParameterGroup)
{-# DEPRECATED dpgDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | Provides the customer-specified description for this DB parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgDescription :: Lens.Lens' DBParameterGroup (Lude.Maybe Lude.Text)
dpgDescription = Lens.lens (description :: DBParameterGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DBParameterGroup)
{-# DEPRECATED dpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML DBParameterGroup where
  parseXML x =
    DBParameterGroup'
      Lude.<$> (x Lude..@? "DBParameterGroupArn")
      Lude.<*> (x Lude..@? "DBParameterGroupFamily")
      Lude.<*> (x Lude..@? "DBParameterGroupName")
      Lude.<*> (x Lude..@? "Description")
