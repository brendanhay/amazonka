-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.EngineAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.EngineAttribute
  ( EngineAttribute (..),

    -- * Smart constructor
    mkEngineAttribute,

    -- * Lenses
    eaValue,
    eaName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A name and value pair that is specific to the engine of the server.
--
-- /See:/ 'mkEngineAttribute' smart constructor.
data EngineAttribute = EngineAttribute'
  { value ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EngineAttribute' with the minimum fields required to make a request.
--
-- * 'name' - The name of the engine attribute.
-- * 'value' - The value of the engine attribute.
mkEngineAttribute ::
  EngineAttribute
mkEngineAttribute =
  EngineAttribute' {value = Lude.Nothing, name = Lude.Nothing}

-- | The value of the engine attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaValue :: Lens.Lens' EngineAttribute (Lude.Maybe (Lude.Sensitive Lude.Text))
eaValue = Lens.lens (value :: EngineAttribute -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {value = a} :: EngineAttribute)
{-# DEPRECATED eaValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the engine attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaName :: Lens.Lens' EngineAttribute (Lude.Maybe Lude.Text)
eaName = Lens.lens (name :: EngineAttribute -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: EngineAttribute)
{-# DEPRECATED eaName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON EngineAttribute where
  parseJSON =
    Lude.withObject
      "EngineAttribute"
      ( \x ->
          EngineAttribute'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Name")
      )

instance Lude.ToJSON EngineAttribute where
  toJSON EngineAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Name" Lude..=) Lude.<$> name]
      )
