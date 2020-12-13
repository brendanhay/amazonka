{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.ConfigurationRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.ConfigurationRevision
  ( ConfigurationRevision (..),

    -- * Smart constructor
    mkConfigurationRevision,

    -- * Lenses
    crCreated,
    crRevision,
    crDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the specified configuration revision.
--
-- /See:/ 'mkConfigurationRevision' smart constructor.
data ConfigurationRevision = ConfigurationRevision'
  { -- | Required. The date and time of the configuration revision.
    created :: Lude.Maybe Lude.Timestamp,
    -- | Required. The revision number of the configuration.
    revision :: Lude.Maybe Lude.Int,
    -- | The description of the configuration revision.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationRevision' with the minimum fields required to make a request.
--
-- * 'created' - Required. The date and time of the configuration revision.
-- * 'revision' - Required. The revision number of the configuration.
-- * 'description' - The description of the configuration revision.
mkConfigurationRevision ::
  ConfigurationRevision
mkConfigurationRevision =
  ConfigurationRevision'
    { created = Lude.Nothing,
      revision = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Required. The date and time of the configuration revision.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCreated :: Lens.Lens' ConfigurationRevision (Lude.Maybe Lude.Timestamp)
crCreated = Lens.lens (created :: ConfigurationRevision -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: ConfigurationRevision)
{-# DEPRECATED crCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Required. The revision number of the configuration.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRevision :: Lens.Lens' ConfigurationRevision (Lude.Maybe Lude.Int)
crRevision = Lens.lens (revision :: ConfigurationRevision -> Lude.Maybe Lude.Int) (\s a -> s {revision = a} :: ConfigurationRevision)
{-# DEPRECATED crRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | The description of the configuration revision.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' ConfigurationRevision (Lude.Maybe Lude.Text)
crDescription = Lens.lens (description :: ConfigurationRevision -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ConfigurationRevision)
{-# DEPRECATED crDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ConfigurationRevision where
  parseJSON =
    Lude.withObject
      "ConfigurationRevision"
      ( \x ->
          ConfigurationRevision'
            Lude.<$> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "revision")
            Lude.<*> (x Lude..:? "description")
      )
