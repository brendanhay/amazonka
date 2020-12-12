{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.UserDataValidationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.UserDataValidationParameters
  ( UserDataValidationParameters (..),

    -- * Smart constructor
    mkUserDataValidationParameters,

    -- * Lenses
    udvpScriptType,
    udvpSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.ScriptType
import Network.AWS.SMS.Types.Source

-- | Contains validation parameters.
--
-- /See:/ 'mkUserDataValidationParameters' smart constructor.
data UserDataValidationParameters = UserDataValidationParameters'
  { scriptType ::
      Lude.Maybe ScriptType,
    source :: Lude.Maybe Source
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserDataValidationParameters' with the minimum fields required to make a request.
--
-- * 'scriptType' - The type of validation script.
-- * 'source' - The location of the validation script.
mkUserDataValidationParameters ::
  UserDataValidationParameters
mkUserDataValidationParameters =
  UserDataValidationParameters'
    { scriptType = Lude.Nothing,
      source = Lude.Nothing
    }

-- | The type of validation script.
--
-- /Note:/ Consider using 'scriptType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvpScriptType :: Lens.Lens' UserDataValidationParameters (Lude.Maybe ScriptType)
udvpScriptType = Lens.lens (scriptType :: UserDataValidationParameters -> Lude.Maybe ScriptType) (\s a -> s {scriptType = a} :: UserDataValidationParameters)
{-# DEPRECATED udvpScriptType "Use generic-lens or generic-optics with 'scriptType' instead." #-}

-- | The location of the validation script.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvpSource :: Lens.Lens' UserDataValidationParameters (Lude.Maybe Source)
udvpSource = Lens.lens (source :: UserDataValidationParameters -> Lude.Maybe Source) (\s a -> s {source = a} :: UserDataValidationParameters)
{-# DEPRECATED udvpSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.FromJSON UserDataValidationParameters where
  parseJSON =
    Lude.withObject
      "UserDataValidationParameters"
      ( \x ->
          UserDataValidationParameters'
            Lude.<$> (x Lude..:? "scriptType") Lude.<*> (x Lude..:? "source")
      )

instance Lude.ToJSON UserDataValidationParameters where
  toJSON UserDataValidationParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("scriptType" Lude..=) Lude.<$> scriptType,
            ("source" Lude..=) Lude.<$> source
          ]
      )
