{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceInformationStringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceInformationStringFilter
  ( InstanceInformationStringFilter (..),

    -- * Smart constructor
    mkInstanceInformationStringFilter,

    -- * Lenses
    iisfValues,
    iisfKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The filters to describe or get information about your managed instances.
--
-- /See:/ 'mkInstanceInformationStringFilter' smart constructor.
data InstanceInformationStringFilter = InstanceInformationStringFilter'
  { -- | The filter values.
    values :: Lude.NonEmpty Lude.Text,
    -- | The filter key name to describe your instances. For example:
    --
    -- "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|"AssociationStatus"|"Tag Key"
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceInformationStringFilter' with the minimum fields required to make a request.
--
-- * 'values' - The filter values.
-- * 'key' - The filter key name to describe your instances. For example:
--
-- "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|"AssociationStatus"|"Tag Key"
mkInstanceInformationStringFilter ::
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  -- | 'key'
  Lude.Text ->
  InstanceInformationStringFilter
mkInstanceInformationStringFilter pValues_ pKey_ =
  InstanceInformationStringFilter' {values = pValues_, key = pKey_}

-- | The filter values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisfValues :: Lens.Lens' InstanceInformationStringFilter (Lude.NonEmpty Lude.Text)
iisfValues = Lens.lens (values :: InstanceInformationStringFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: InstanceInformationStringFilter)
{-# DEPRECATED iisfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The filter key name to describe your instances. For example:
--
-- "InstanceIds"|"AgentVersion"|"PingStatus"|"PlatformTypes"|"ActivationIds"|"IamRole"|"ResourceType"|"AssociationStatus"|"Tag Key"
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iisfKey :: Lens.Lens' InstanceInformationStringFilter Lude.Text
iisfKey = Lens.lens (key :: InstanceInformationStringFilter -> Lude.Text) (\s a -> s {key = a} :: InstanceInformationStringFilter)
{-# DEPRECATED iisfKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON InstanceInformationStringFilter where
  toJSON InstanceInformationStringFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Values" Lude..= values),
            Lude.Just ("Key" Lude..= key)
          ]
      )
