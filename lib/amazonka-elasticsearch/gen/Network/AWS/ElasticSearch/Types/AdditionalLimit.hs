{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdditionalLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.AdditionalLimit
  ( AdditionalLimit (..)
  -- * Smart constructor
  , mkAdditionalLimit
  -- * Lenses
  , alLimitName
  , alLimitValues
  ) where

import qualified Network.AWS.ElasticSearch.Types.LimitName as Types
import qualified Network.AWS.ElasticSearch.Types.LimitValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | List of limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ . 
--
-- /See:/ 'mkAdditionalLimit' smart constructor.
data AdditionalLimit = AdditionalLimit'
  { limitName :: Core.Maybe Types.LimitName
    -- ^ Name of Additional Limit is specific to a given InstanceType and for each of it's @'InstanceRole' @ etc. 
--
-- Attributes and their details: 
--
--     * MaximumNumberOfDataNodesSupported
-- This attribute will be present in Master node only to specify how much data nodes upto which given @'ESPartitionInstanceType' @ can support as master node. 
--     * MaximumNumberOfDataNodesWithoutMasterNode
-- This attribute will be present in Data node only to specify how much data nodes of given @'ESPartitionInstanceType' @ upto which you don't need any master nodes to govern them. 
--
  , limitValues :: Core.Maybe [Types.LimitValue]
    -- ^ Value for given @'AdditionalLimit$LimitName' @ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdditionalLimit' value with any optional fields omitted.
mkAdditionalLimit
    :: AdditionalLimit
mkAdditionalLimit
  = AdditionalLimit'{limitName = Core.Nothing,
                     limitValues = Core.Nothing}

-- | Name of Additional Limit is specific to a given InstanceType and for each of it's @'InstanceRole' @ etc. 
--
-- Attributes and their details: 
--
--     * MaximumNumberOfDataNodesSupported
-- This attribute will be present in Master node only to specify how much data nodes upto which given @'ESPartitionInstanceType' @ can support as master node. 
--     * MaximumNumberOfDataNodesWithoutMasterNode
-- This attribute will be present in Data node only to specify how much data nodes of given @'ESPartitionInstanceType' @ upto which you don't need any master nodes to govern them. 
--
--
-- /Note:/ Consider using 'limitName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alLimitName :: Lens.Lens' AdditionalLimit (Core.Maybe Types.LimitName)
alLimitName = Lens.field @"limitName"
{-# INLINEABLE alLimitName #-}
{-# DEPRECATED limitName "Use generic-lens or generic-optics with 'limitName' instead"  #-}

-- | Value for given @'AdditionalLimit$LimitName' @ . 
--
-- /Note:/ Consider using 'limitValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alLimitValues :: Lens.Lens' AdditionalLimit (Core.Maybe [Types.LimitValue])
alLimitValues = Lens.field @"limitValues"
{-# INLINEABLE alLimitValues #-}
{-# DEPRECATED limitValues "Use generic-lens or generic-optics with 'limitValues' instead"  #-}

instance Core.FromJSON AdditionalLimit where
        parseJSON
          = Core.withObject "AdditionalLimit" Core.$
              \ x ->
                AdditionalLimit' Core.<$>
                  (x Core..:? "LimitName") Core.<*> x Core..:? "LimitValues"
