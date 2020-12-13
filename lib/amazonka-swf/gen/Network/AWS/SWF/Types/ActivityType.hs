{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityType
  ( ActivityType (..),

    -- * Smart constructor
    mkActivityType,

    -- * Lenses
    atName,
    atVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an activity type.
--
-- /See:/ 'mkActivityType' smart constructor.
data ActivityType = ActivityType'
  { -- | The name of this activity.
    name :: Lude.Text,
    -- | The version of this activity.
    version :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivityType' with the minimum fields required to make a request.
--
-- * 'name' - The name of this activity.
-- * 'version' - The version of this activity.
mkActivityType ::
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  ActivityType
mkActivityType pName_ pVersion_ =
  ActivityType' {name = pName_, version = pVersion_}

-- | The name of this activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atName :: Lens.Lens' ActivityType Lude.Text
atName = Lens.lens (name :: ActivityType -> Lude.Text) (\s a -> s {name = a} :: ActivityType)
{-# DEPRECATED atName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of this activity.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atVersion :: Lens.Lens' ActivityType Lude.Text
atVersion = Lens.lens (version :: ActivityType -> Lude.Text) (\s a -> s {version = a} :: ActivityType)
{-# DEPRECATED atVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.FromJSON ActivityType where
  parseJSON =
    Lude.withObject
      "ActivityType"
      ( \x ->
          ActivityType'
            Lude.<$> (x Lude..: "name") Lude.<*> (x Lude..: "version")
      )

instance Lude.ToJSON ActivityType where
  toJSON ActivityType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("version" Lude..= version)
          ]
      )
