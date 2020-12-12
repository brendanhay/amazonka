{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ServerValidationOutput
  ( ServerValidationOutput (..),

    -- * Smart constructor
    mkServerValidationOutput,

    -- * Lenses
    svoServer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.Server

-- | Contains output from validating an instance.
--
-- /See:/ 'mkServerValidationOutput' smart constructor.
newtype ServerValidationOutput = ServerValidationOutput'
  { server ::
      Lude.Maybe Server
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerValidationOutput' with the minimum fields required to make a request.
--
-- * 'server' - Undocumented field.
mkServerValidationOutput ::
  ServerValidationOutput
mkServerValidationOutput =
  ServerValidationOutput' {server = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svoServer :: Lens.Lens' ServerValidationOutput (Lude.Maybe Server)
svoServer = Lens.lens (server :: ServerValidationOutput -> Lude.Maybe Server) (\s a -> s {server = a} :: ServerValidationOutput)
{-# DEPRECATED svoServer "Use generic-lens or generic-optics with 'server' instead." #-}

instance Lude.FromJSON ServerValidationOutput where
  parseJSON =
    Lude.withObject
      "ServerValidationOutput"
      (\x -> ServerValidationOutput' Lude.<$> (x Lude..:? "server"))
