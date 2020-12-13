{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.EgressAccessLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.EgressAccessLogs
  ( EgressAccessLogs (..),

    -- * Smart constructor
    mkEgressAccessLogs,

    -- * Lenses
    ealLogGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configure egress access logging.
--
-- /See:/ 'mkEgressAccessLogs' smart constructor.
newtype EgressAccessLogs = EgressAccessLogs'
  { -- | Customize the log group name.
    logGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EgressAccessLogs' with the minimum fields required to make a request.
--
-- * 'logGroupName' - Customize the log group name.
mkEgressAccessLogs ::
  EgressAccessLogs
mkEgressAccessLogs = EgressAccessLogs' {logGroupName = Lude.Nothing}

-- | Customize the log group name.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ealLogGroupName :: Lens.Lens' EgressAccessLogs (Lude.Maybe Lude.Text)
ealLogGroupName = Lens.lens (logGroupName :: EgressAccessLogs -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: EgressAccessLogs)
{-# DEPRECATED ealLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

instance Lude.FromJSON EgressAccessLogs where
  parseJSON =
    Lude.withObject
      "EgressAccessLogs"
      (\x -> EgressAccessLogs' Lude.<$> (x Lude..:? "logGroupName"))

instance Lude.ToJSON EgressAccessLogs where
  toJSON EgressAccessLogs' {..} =
    Lude.object
      (Lude.catMaybes [("logGroupName" Lude..=) Lude.<$> logGroupName])
