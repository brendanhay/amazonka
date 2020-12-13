{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BootstrapActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BootstrapActionConfig
  ( BootstrapActionConfig (..),

    -- * Smart constructor
    mkBootstrapActionConfig,

    -- * Lenses
    bacName,
    bacScriptBootstrapAction,
  )
where

import Network.AWS.EMR.Types.ScriptBootstrapActionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration of a bootstrap action.
--
-- /See:/ 'mkBootstrapActionConfig' smart constructor.
data BootstrapActionConfig = BootstrapActionConfig'
  { -- | The name of the bootstrap action.
    name :: Lude.Text,
    -- | The script run by the bootstrap action.
    scriptBootstrapAction :: ScriptBootstrapActionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BootstrapActionConfig' with the minimum fields required to make a request.
--
-- * 'name' - The name of the bootstrap action.
-- * 'scriptBootstrapAction' - The script run by the bootstrap action.
mkBootstrapActionConfig ::
  -- | 'name'
  Lude.Text ->
  -- | 'scriptBootstrapAction'
  ScriptBootstrapActionConfig ->
  BootstrapActionConfig
mkBootstrapActionConfig pName_ pScriptBootstrapAction_ =
  BootstrapActionConfig'
    { name = pName_,
      scriptBootstrapAction = pScriptBootstrapAction_
    }

-- | The name of the bootstrap action.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bacName :: Lens.Lens' BootstrapActionConfig Lude.Text
bacName = Lens.lens (name :: BootstrapActionConfig -> Lude.Text) (\s a -> s {name = a} :: BootstrapActionConfig)
{-# DEPRECATED bacName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The script run by the bootstrap action.
--
-- /Note:/ Consider using 'scriptBootstrapAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bacScriptBootstrapAction :: Lens.Lens' BootstrapActionConfig ScriptBootstrapActionConfig
bacScriptBootstrapAction = Lens.lens (scriptBootstrapAction :: BootstrapActionConfig -> ScriptBootstrapActionConfig) (\s a -> s {scriptBootstrapAction = a} :: BootstrapActionConfig)
{-# DEPRECATED bacScriptBootstrapAction "Use generic-lens or generic-optics with 'scriptBootstrapAction' instead." #-}

instance Lude.ToJSON BootstrapActionConfig where
  toJSON BootstrapActionConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ScriptBootstrapAction" Lude..= scriptBootstrapAction)
          ]
      )
