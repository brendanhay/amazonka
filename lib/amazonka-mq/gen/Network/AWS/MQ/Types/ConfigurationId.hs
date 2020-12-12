{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.ConfigurationId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.ConfigurationId
  ( ConfigurationId (..),

    -- * Smart constructor
    mkConfigurationId,

    -- * Lenses
    ciId,
    ciRevision,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /Important:/ Does not apply to RabbitMQ brokers.
--
-- /See:/ 'mkConfigurationId' smart constructor.
data ConfigurationId = ConfigurationId'
  { id :: Lude.Maybe Lude.Text,
    revision :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationId' with the minimum fields required to make a request.
--
-- * 'id' - Required. The unique ID that Amazon MQ generates for the configuration.
-- * 'revision' - The revision number of the configuration.
mkConfigurationId ::
  ConfigurationId
mkConfigurationId =
  ConfigurationId' {id = Lude.Nothing, revision = Lude.Nothing}

-- | Required. The unique ID that Amazon MQ generates for the configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciId :: Lens.Lens' ConfigurationId (Lude.Maybe Lude.Text)
ciId = Lens.lens (id :: ConfigurationId -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ConfigurationId)
{-# DEPRECATED ciId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The revision number of the configuration.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciRevision :: Lens.Lens' ConfigurationId (Lude.Maybe Lude.Int)
ciRevision = Lens.lens (revision :: ConfigurationId -> Lude.Maybe Lude.Int) (\s a -> s {revision = a} :: ConfigurationId)
{-# DEPRECATED ciRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

instance Lude.FromJSON ConfigurationId where
  parseJSON =
    Lude.withObject
      "ConfigurationId"
      ( \x ->
          ConfigurationId'
            Lude.<$> (x Lude..:? "id") Lude.<*> (x Lude..:? "revision")
      )

instance Lude.ToJSON ConfigurationId where
  toJSON ConfigurationId' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("id" Lude..=) Lude.<$> id,
            ("revision" Lude..=) Lude.<$> revision
          ]
      )
