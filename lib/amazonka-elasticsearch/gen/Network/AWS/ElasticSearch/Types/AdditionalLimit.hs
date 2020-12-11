-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdditionalLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdditionalLimit
  ( AdditionalLimit (..),

    -- * Smart constructor
    mkAdditionalLimit,

    -- * Lenses
    alLimitName,
    alLimitValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | List of limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ .
--
-- /See:/ 'mkAdditionalLimit' smart constructor.
data AdditionalLimit = AdditionalLimit'
  { limitName ::
      Lude.Maybe Lude.Text,
    limitValues :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdditionalLimit' with the minimum fields required to make a request.
--
-- * 'limitName' - Name of Additional Limit is specific to a given InstanceType and for each of it's @'InstanceRole' @ etc.
--
-- Attributes and their details:
--
--     * MaximumNumberOfDataNodesSupported
-- This attribute will be present in Master node only to specify how much data nodes upto which given @'ESPartitionInstanceType' @ can support as master node.
--     * MaximumNumberOfDataNodesWithoutMasterNode
-- This attribute will be present in Data node only to specify how much data nodes of given @'ESPartitionInstanceType' @ upto which you don't need any master nodes to govern them.
--
-- * 'limitValues' - Value for given @'AdditionalLimit$LimitName' @ .
mkAdditionalLimit ::
  AdditionalLimit
mkAdditionalLimit =
  AdditionalLimit'
    { limitName = Lude.Nothing,
      limitValues = Lude.Nothing
    }

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
alLimitName :: Lens.Lens' AdditionalLimit (Lude.Maybe Lude.Text)
alLimitName = Lens.lens (limitName :: AdditionalLimit -> Lude.Maybe Lude.Text) (\s a -> s {limitName = a} :: AdditionalLimit)
{-# DEPRECATED alLimitName "Use generic-lens or generic-optics with 'limitName' instead." #-}

-- | Value for given @'AdditionalLimit$LimitName' @ .
--
-- /Note:/ Consider using 'limitValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alLimitValues :: Lens.Lens' AdditionalLimit (Lude.Maybe [Lude.Text])
alLimitValues = Lens.lens (limitValues :: AdditionalLimit -> Lude.Maybe [Lude.Text]) (\s a -> s {limitValues = a} :: AdditionalLimit)
{-# DEPRECATED alLimitValues "Use generic-lens or generic-optics with 'limitValues' instead." #-}

instance Lude.FromJSON AdditionalLimit where
  parseJSON =
    Lude.withObject
      "AdditionalLimit"
      ( \x ->
          AdditionalLimit'
            Lude.<$> (x Lude..:? "LimitName")
            Lude.<*> (x Lude..:? "LimitValues" Lude..!= Lude.mempty)
      )
