-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DeltaSyncConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DeltaSyncConfig
  ( DeltaSyncConfig (..),

    -- * Smart constructor
    mkDeltaSyncConfig,

    -- * Lenses
    dscBaseTableTTL,
    dscDeltaSyncTableName,
    dscDeltaSyncTableTTL,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Delta Sync configuration.
--
-- /See:/ 'mkDeltaSyncConfig' smart constructor.
data DeltaSyncConfig = DeltaSyncConfig'
  { baseTableTTL ::
      Lude.Maybe Lude.Integer,
    deltaSyncTableName :: Lude.Maybe Lude.Text,
    deltaSyncTableTTL :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeltaSyncConfig' with the minimum fields required to make a request.
--
-- * 'baseTableTTL' - The number of minutes an Item is stored in the datasource.
-- * 'deltaSyncTableName' - The Delta Sync table name.
-- * 'deltaSyncTableTTL' - The number of minutes a Delta Sync log entry is stored in the Delta Sync table.
mkDeltaSyncConfig ::
  DeltaSyncConfig
mkDeltaSyncConfig =
  DeltaSyncConfig'
    { baseTableTTL = Lude.Nothing,
      deltaSyncTableName = Lude.Nothing,
      deltaSyncTableTTL = Lude.Nothing
    }

-- | The number of minutes an Item is stored in the datasource.
--
-- /Note:/ Consider using 'baseTableTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscBaseTableTTL :: Lens.Lens' DeltaSyncConfig (Lude.Maybe Lude.Integer)
dscBaseTableTTL = Lens.lens (baseTableTTL :: DeltaSyncConfig -> Lude.Maybe Lude.Integer) (\s a -> s {baseTableTTL = a} :: DeltaSyncConfig)
{-# DEPRECATED dscBaseTableTTL "Use generic-lens or generic-optics with 'baseTableTTL' instead." #-}

-- | The Delta Sync table name.
--
-- /Note:/ Consider using 'deltaSyncTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscDeltaSyncTableName :: Lens.Lens' DeltaSyncConfig (Lude.Maybe Lude.Text)
dscDeltaSyncTableName = Lens.lens (deltaSyncTableName :: DeltaSyncConfig -> Lude.Maybe Lude.Text) (\s a -> s {deltaSyncTableName = a} :: DeltaSyncConfig)
{-# DEPRECATED dscDeltaSyncTableName "Use generic-lens or generic-optics with 'deltaSyncTableName' instead." #-}

-- | The number of minutes a Delta Sync log entry is stored in the Delta Sync table.
--
-- /Note:/ Consider using 'deltaSyncTableTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscDeltaSyncTableTTL :: Lens.Lens' DeltaSyncConfig (Lude.Maybe Lude.Integer)
dscDeltaSyncTableTTL = Lens.lens (deltaSyncTableTTL :: DeltaSyncConfig -> Lude.Maybe Lude.Integer) (\s a -> s {deltaSyncTableTTL = a} :: DeltaSyncConfig)
{-# DEPRECATED dscDeltaSyncTableTTL "Use generic-lens or generic-optics with 'deltaSyncTableTTL' instead." #-}

instance Lude.FromJSON DeltaSyncConfig where
  parseJSON =
    Lude.withObject
      "DeltaSyncConfig"
      ( \x ->
          DeltaSyncConfig'
            Lude.<$> (x Lude..:? "baseTableTTL")
            Lude.<*> (x Lude..:? "deltaSyncTableName")
            Lude.<*> (x Lude..:? "deltaSyncTableTTL")
      )

instance Lude.ToJSON DeltaSyncConfig where
  toJSON DeltaSyncConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("baseTableTTL" Lude..=) Lude.<$> baseTableTTL,
            ("deltaSyncTableName" Lude..=) Lude.<$> deltaSyncTableName,
            ("deltaSyncTableTTL" Lude..=) Lude.<$> deltaSyncTableTTL
          ]
      )
