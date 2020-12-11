-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditCheckConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditCheckConfiguration
  ( AuditCheckConfiguration (..),

    -- * Smart constructor
    mkAuditCheckConfiguration,

    -- * Lenses
    accEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Which audit checks are enabled and disabled for this account.
--
-- /See:/ 'mkAuditCheckConfiguration' smart constructor.
newtype AuditCheckConfiguration = AuditCheckConfiguration'
  { enabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuditCheckConfiguration' with the minimum fields required to make a request.
--
-- * 'enabled' - True if this audit check is enabled for this account.
mkAuditCheckConfiguration ::
  AuditCheckConfiguration
mkAuditCheckConfiguration =
  AuditCheckConfiguration' {enabled = Lude.Nothing}

-- | True if this audit check is enabled for this account.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
accEnabled :: Lens.Lens' AuditCheckConfiguration (Lude.Maybe Lude.Bool)
accEnabled = Lens.lens (enabled :: AuditCheckConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: AuditCheckConfiguration)
{-# DEPRECATED accEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromJSON AuditCheckConfiguration where
  parseJSON =
    Lude.withObject
      "AuditCheckConfiguration"
      (\x -> AuditCheckConfiguration' Lude.<$> (x Lude..:? "enabled"))

instance Lude.ToJSON AuditCheckConfiguration where
  toJSON AuditCheckConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [("enabled" Lude..=) Lude.<$> enabled])
