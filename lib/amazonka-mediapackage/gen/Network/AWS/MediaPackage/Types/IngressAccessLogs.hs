{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.IngressAccessLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.IngressAccessLogs
  ( IngressAccessLogs (..),

    -- * Smart constructor
    mkIngressAccessLogs,

    -- * Lenses
    ialLogGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configure ingress access logging.
--
-- /See:/ 'mkIngressAccessLogs' smart constructor.
newtype IngressAccessLogs = IngressAccessLogs'
  { -- | Customize the log group name.
    logGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IngressAccessLogs' with the minimum fields required to make a request.
--
-- * 'logGroupName' - Customize the log group name.
mkIngressAccessLogs ::
  IngressAccessLogs
mkIngressAccessLogs =
  IngressAccessLogs' {logGroupName = Lude.Nothing}

-- | Customize the log group name.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ialLogGroupName :: Lens.Lens' IngressAccessLogs (Lude.Maybe Lude.Text)
ialLogGroupName = Lens.lens (logGroupName :: IngressAccessLogs -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: IngressAccessLogs)
{-# DEPRECATED ialLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.FromJSON IngressAccessLogs where
  parseJSON =
    Lude.withObject
      "IngressAccessLogs"
      (\x -> IngressAccessLogs' Lude.<$> (x Lude..:? "logGroupName"))

instance Lude.ToJSON IngressAccessLogs where
  toJSON IngressAccessLogs' {..} =
    Lude.object
      (Lude.catMaybes [("logGroupName" Lude..=) Lude.<$> logGroupName])
