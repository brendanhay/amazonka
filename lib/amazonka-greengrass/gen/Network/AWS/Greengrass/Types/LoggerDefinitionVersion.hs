{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LoggerDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerDefinitionVersion
  ( LoggerDefinitionVersion (..),

    -- * Smart constructor
    mkLoggerDefinitionVersion,

    -- * Lenses
    ldvLoggers,
  )
where

import Network.AWS.Greengrass.Types.GreengrassLogger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a logger definition version.
--
-- /See:/ 'mkLoggerDefinitionVersion' smart constructor.
newtype LoggerDefinitionVersion = LoggerDefinitionVersion'
  { loggers ::
      Lude.Maybe [GreengrassLogger]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggerDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'loggers' - A list of loggers.
mkLoggerDefinitionVersion ::
  LoggerDefinitionVersion
mkLoggerDefinitionVersion =
  LoggerDefinitionVersion' {loggers = Lude.Nothing}

-- | A list of loggers.
--
-- /Note:/ Consider using 'loggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvLoggers :: Lens.Lens' LoggerDefinitionVersion (Lude.Maybe [GreengrassLogger])
ldvLoggers = Lens.lens (loggers :: LoggerDefinitionVersion -> Lude.Maybe [GreengrassLogger]) (\s a -> s {loggers = a} :: LoggerDefinitionVersion)
{-# DEPRECATED ldvLoggers "Use generic-lens or generic-optics with 'loggers' instead." #-}

instance Lude.FromJSON LoggerDefinitionVersion where
  parseJSON =
    Lude.withObject
      "LoggerDefinitionVersion"
      ( \x ->
          LoggerDefinitionVersion'
            Lude.<$> (x Lude..:? "Loggers" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON LoggerDefinitionVersion where
  toJSON LoggerDefinitionVersion' {..} =
    Lude.object
      (Lude.catMaybes [("Loggers" Lude..=) Lude.<$> loggers])
