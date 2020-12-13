{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
  ( CacheNodeTypeSpecificParameter (..),

    -- * Smart constructor
    mkCacheNodeTypeSpecificParameter,

    -- * Lenses
    cntspCacheNodeTypeSpecificValues,
    cntspMinimumEngineVersion,
    cntspSource,
    cntspIsModifiable,
    cntspDataType,
    cntspAllowedValues,
    cntspParameterName,
    cntspDescription,
    cntspChangeType,
  )
where

import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
import Network.AWS.ElastiCache.Types.ChangeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A parameter that has a different value for each cache node type it is applied to. For example, in a Redis cluster, a @cache.m1.large@ cache node type would have a larger @maxmemory@ value than a @cache.m1.small@ type.
--
-- /See:/ 'mkCacheNodeTypeSpecificParameter' smart constructor.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter'
  { -- | A list of cache node types and their corresponding values for this parameter.
    cacheNodeTypeSpecificValues :: Lude.Maybe [CacheNodeTypeSpecificValue],
    -- | The earliest cache engine version to which the parameter can apply.
    minimumEngineVersion :: Lude.Maybe Lude.Text,
    -- | The source of the parameter value.
    source :: Lude.Maybe Lude.Text,
    -- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
    isModifiable :: Lude.Maybe Lude.Bool,
    -- | The valid data type for the parameter.
    dataType :: Lude.Maybe Lude.Text,
    -- | The valid range of values for the parameter.
    allowedValues :: Lude.Maybe Lude.Text,
    -- | The name of the parameter.
    parameterName :: Lude.Maybe Lude.Text,
    -- | A description of the parameter.
    description :: Lude.Maybe Lude.Text,
    -- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
    changeType :: Lude.Maybe ChangeType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheNodeTypeSpecificParameter' with the minimum fields required to make a request.
--
-- * 'cacheNodeTypeSpecificValues' - A list of cache node types and their corresponding values for this parameter.
-- * 'minimumEngineVersion' - The earliest cache engine version to which the parameter can apply.
-- * 'source' - The source of the parameter value.
-- * 'isModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
-- * 'dataType' - The valid data type for the parameter.
-- * 'allowedValues' - The valid range of values for the parameter.
-- * 'parameterName' - The name of the parameter.
-- * 'description' - A description of the parameter.
-- * 'changeType' - Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
mkCacheNodeTypeSpecificParameter ::
  CacheNodeTypeSpecificParameter
mkCacheNodeTypeSpecificParameter =
  CacheNodeTypeSpecificParameter'
    { cacheNodeTypeSpecificValues =
        Lude.Nothing,
      minimumEngineVersion = Lude.Nothing,
      source = Lude.Nothing,
      isModifiable = Lude.Nothing,
      dataType = Lude.Nothing,
      allowedValues = Lude.Nothing,
      parameterName = Lude.Nothing,
      description = Lude.Nothing,
      changeType = Lude.Nothing
    }

-- | A list of cache node types and their corresponding values for this parameter.
--
-- /Note:/ Consider using 'cacheNodeTypeSpecificValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspCacheNodeTypeSpecificValues :: Lens.Lens' CacheNodeTypeSpecificParameter (Lude.Maybe [CacheNodeTypeSpecificValue])
cntspCacheNodeTypeSpecificValues = Lens.lens (cacheNodeTypeSpecificValues :: CacheNodeTypeSpecificParameter -> Lude.Maybe [CacheNodeTypeSpecificValue]) (\s a -> s {cacheNodeTypeSpecificValues = a} :: CacheNodeTypeSpecificParameter)
{-# DEPRECATED cntspCacheNodeTypeSpecificValues "Use generic-lens or generic-optics with 'cacheNodeTypeSpecificValues' instead." #-}

-- | The earliest cache engine version to which the parameter can apply.
--
-- /Note:/ Consider using 'minimumEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspMinimumEngineVersion :: Lens.Lens' CacheNodeTypeSpecificParameter (Lude.Maybe Lude.Text)
cntspMinimumEngineVersion = Lens.lens (minimumEngineVersion :: CacheNodeTypeSpecificParameter -> Lude.Maybe Lude.Text) (\s a -> s {minimumEngineVersion = a} :: CacheNodeTypeSpecificParameter)
{-# DEPRECATED cntspMinimumEngineVersion "Use generic-lens or generic-optics with 'minimumEngineVersion' instead." #-}

-- | The source of the parameter value.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspSource :: Lens.Lens' CacheNodeTypeSpecificParameter (Lude.Maybe Lude.Text)
cntspSource = Lens.lens (source :: CacheNodeTypeSpecificParameter -> Lude.Maybe Lude.Text) (\s a -> s {source = a} :: CacheNodeTypeSpecificParameter)
{-# DEPRECATED cntspSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- /Note:/ Consider using 'isModifiable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspIsModifiable :: Lens.Lens' CacheNodeTypeSpecificParameter (Lude.Maybe Lude.Bool)
cntspIsModifiable = Lens.lens (isModifiable :: CacheNodeTypeSpecificParameter -> Lude.Maybe Lude.Bool) (\s a -> s {isModifiable = a} :: CacheNodeTypeSpecificParameter)
{-# DEPRECATED cntspIsModifiable "Use generic-lens or generic-optics with 'isModifiable' instead." #-}

-- | The valid data type for the parameter.
--
-- /Note:/ Consider using 'dataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspDataType :: Lens.Lens' CacheNodeTypeSpecificParameter (Lude.Maybe Lude.Text)
cntspDataType = Lens.lens (dataType :: CacheNodeTypeSpecificParameter -> Lude.Maybe Lude.Text) (\s a -> s {dataType = a} :: CacheNodeTypeSpecificParameter)
{-# DEPRECATED cntspDataType "Use generic-lens or generic-optics with 'dataType' instead." #-}

-- | The valid range of values for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspAllowedValues :: Lens.Lens' CacheNodeTypeSpecificParameter (Lude.Maybe Lude.Text)
cntspAllowedValues = Lens.lens (allowedValues :: CacheNodeTypeSpecificParameter -> Lude.Maybe Lude.Text) (\s a -> s {allowedValues = a} :: CacheNodeTypeSpecificParameter)
{-# DEPRECATED cntspAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspParameterName :: Lens.Lens' CacheNodeTypeSpecificParameter (Lude.Maybe Lude.Text)
cntspParameterName = Lens.lens (parameterName :: CacheNodeTypeSpecificParameter -> Lude.Maybe Lude.Text) (\s a -> s {parameterName = a} :: CacheNodeTypeSpecificParameter)
{-# DEPRECATED cntspParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | A description of the parameter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspDescription :: Lens.Lens' CacheNodeTypeSpecificParameter (Lude.Maybe Lude.Text)
cntspDescription = Lens.lens (description :: CacheNodeTypeSpecificParameter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CacheNodeTypeSpecificParameter)
{-# DEPRECATED cntspDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
--
-- /Note:/ Consider using 'changeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cntspChangeType :: Lens.Lens' CacheNodeTypeSpecificParameter (Lude.Maybe ChangeType)
cntspChangeType = Lens.lens (changeType :: CacheNodeTypeSpecificParameter -> Lude.Maybe ChangeType) (\s a -> s {changeType = a} :: CacheNodeTypeSpecificParameter)
{-# DEPRECATED cntspChangeType "Use generic-lens or generic-optics with 'changeType' instead." #-}

instance Lude.FromXML CacheNodeTypeSpecificParameter where
  parseXML x =
    CacheNodeTypeSpecificParameter'
      Lude.<$> ( x Lude..@? "CacheNodeTypeSpecificValues" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CacheNodeTypeSpecificValue")
               )
      Lude.<*> (x Lude..@? "MinimumEngineVersion")
      Lude.<*> (x Lude..@? "Source")
      Lude.<*> (x Lude..@? "IsModifiable")
      Lude.<*> (x Lude..@? "DataType")
      Lude.<*> (x Lude..@? "AllowedValues")
      Lude.<*> (x Lude..@? "ParameterName")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> (x Lude..@? "ChangeType")
