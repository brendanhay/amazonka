-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ExecutionEngineConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ExecutionEngineConfig
  ( ExecutionEngineConfig (..),

    -- * Smart constructor
    mkExecutionEngineConfig,

    -- * Lenses
    eecMasterInstanceSecurityGroupId,
    eecType,
    eecId,
  )
where

import Network.AWS.EMR.Types.ExecutionEngineType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the execution engine (cluster) to run the notebook and perform the notebook execution, for example, an EMR cluster.
--
-- /See:/ 'mkExecutionEngineConfig' smart constructor.
data ExecutionEngineConfig = ExecutionEngineConfig'
  { masterInstanceSecurityGroupId ::
      Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ExecutionEngineType,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExecutionEngineConfig' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier of the execution engine. For an EMR cluster, this is the cluster ID.
-- * 'masterInstanceSecurityGroupId' - An optional unique ID of an EC2 security group to associate with the master instance of the EMR cluster for this notebook execution. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
-- * 'type'' - The type of execution engine. A value of @EMR@ specifies an EMR cluster.
mkExecutionEngineConfig ::
  -- | 'id'
  Lude.Text ->
  ExecutionEngineConfig
mkExecutionEngineConfig pId_ =
  ExecutionEngineConfig'
    { masterInstanceSecurityGroupId =
        Lude.Nothing,
      type' = Lude.Nothing,
      id = pId_
    }

-- | An optional unique ID of an EC2 security group to associate with the master instance of the EMR cluster for this notebook execution. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
--
-- /Note:/ Consider using 'masterInstanceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eecMasterInstanceSecurityGroupId :: Lens.Lens' ExecutionEngineConfig (Lude.Maybe Lude.Text)
eecMasterInstanceSecurityGroupId = Lens.lens (masterInstanceSecurityGroupId :: ExecutionEngineConfig -> Lude.Maybe Lude.Text) (\s a -> s {masterInstanceSecurityGroupId = a} :: ExecutionEngineConfig)
{-# DEPRECATED eecMasterInstanceSecurityGroupId "Use generic-lens or generic-optics with 'masterInstanceSecurityGroupId' instead." #-}

-- | The type of execution engine. A value of @EMR@ specifies an EMR cluster.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eecType :: Lens.Lens' ExecutionEngineConfig (Lude.Maybe ExecutionEngineType)
eecType = Lens.lens (type' :: ExecutionEngineConfig -> Lude.Maybe ExecutionEngineType) (\s a -> s {type' = a} :: ExecutionEngineConfig)
{-# DEPRECATED eecType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The unique identifier of the execution engine. For an EMR cluster, this is the cluster ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eecId :: Lens.Lens' ExecutionEngineConfig Lude.Text
eecId = Lens.lens (id :: ExecutionEngineConfig -> Lude.Text) (\s a -> s {id = a} :: ExecutionEngineConfig)
{-# DEPRECATED eecId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON ExecutionEngineConfig where
  parseJSON =
    Lude.withObject
      "ExecutionEngineConfig"
      ( \x ->
          ExecutionEngineConfig'
            Lude.<$> (x Lude..:? "MasterInstanceSecurityGroupId")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..: "Id")
      )

instance Lude.ToJSON ExecutionEngineConfig where
  toJSON ExecutionEngineConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MasterInstanceSecurityGroupId" Lude..=)
              Lude.<$> masterInstanceSecurityGroupId,
            ("Type" Lude..=) Lude.<$> type',
            Lude.Just ("Id" Lude..= id)
          ]
      )
