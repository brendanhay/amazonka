{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Change
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Change
  ( Change (..),

    -- * Smart constructor
    mkChange,

    -- * Lenses
    cResourceChange,
    cType,
  )
where

import Network.AWS.CloudFormation.Types.ChangeType
import Network.AWS.CloudFormation.Types.ResourceChange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @Change@ structure describes the changes AWS CloudFormation will perform if you execute the change set.
--
-- /See:/ 'mkChange' smart constructor.
data Change = Change'
  { -- | A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
    resourceChange :: Lude.Maybe ResourceChange,
    -- | The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
    type' :: Lude.Maybe ChangeType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Change' with the minimum fields required to make a request.
--
-- * 'resourceChange' - A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
-- * 'type'' - The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
mkChange ::
  Change
mkChange =
  Change' {resourceChange = Lude.Nothing, type' = Lude.Nothing}

-- | A @ResourceChange@ structure that describes the resource and action that AWS CloudFormation will perform.
--
-- /Note:/ Consider using 'resourceChange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cResourceChange :: Lens.Lens' Change (Lude.Maybe ResourceChange)
cResourceChange = Lens.lens (resourceChange :: Change -> Lude.Maybe ResourceChange) (\s a -> s {resourceChange = a} :: Change)
{-# DEPRECATED cResourceChange "Use generic-lens or generic-optics with 'resourceChange' instead." #-}

-- | The type of entity that AWS CloudFormation changes. Currently, the only entity type is @Resource@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cType :: Lens.Lens' Change (Lude.Maybe ChangeType)
cType = Lens.lens (type' :: Change -> Lude.Maybe ChangeType) (\s a -> s {type' = a} :: Change)
{-# DEPRECATED cType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML Change where
  parseXML x =
    Change'
      Lude.<$> (x Lude..@? "ResourceChange") Lude.<*> (x Lude..@? "Type")
