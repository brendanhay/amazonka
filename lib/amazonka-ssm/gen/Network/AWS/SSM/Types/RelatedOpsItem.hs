{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.RelatedOpsItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.RelatedOpsItem
  ( RelatedOpsItem (..),

    -- * Smart constructor
    mkRelatedOpsItem,

    -- * Lenses
    roiOpsItemId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An OpsItems that shares something in common with the current OpsItem. For example, related OpsItems can include OpsItems with similar error messages, impacted resources, or statuses for the impacted resource.
--
-- /See:/ 'mkRelatedOpsItem' smart constructor.
newtype RelatedOpsItem = RelatedOpsItem' {opsItemId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RelatedOpsItem' with the minimum fields required to make a request.
--
-- * 'opsItemId' - The ID of an OpsItem related to the current OpsItem.
mkRelatedOpsItem ::
  -- | 'opsItemId'
  Lude.Text ->
  RelatedOpsItem
mkRelatedOpsItem pOpsItemId_ =
  RelatedOpsItem' {opsItemId = pOpsItemId_}

-- | The ID of an OpsItem related to the current OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roiOpsItemId :: Lens.Lens' RelatedOpsItem Lude.Text
roiOpsItemId = Lens.lens (opsItemId :: RelatedOpsItem -> Lude.Text) (\s a -> s {opsItemId = a} :: RelatedOpsItem)
{-# DEPRECATED roiOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

instance Lude.FromJSON RelatedOpsItem where
  parseJSON =
    Lude.withObject
      "RelatedOpsItem"
      (\x -> RelatedOpsItem' Lude.<$> (x Lude..: "OpsItemId"))

instance Lude.ToJSON RelatedOpsItem where
  toJSON RelatedOpsItem' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("OpsItemId" Lude..= opsItemId)])
