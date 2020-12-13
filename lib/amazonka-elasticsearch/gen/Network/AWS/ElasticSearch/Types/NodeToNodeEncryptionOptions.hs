{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
  ( NodeToNodeEncryptionOptions (..),

    -- * Smart constructor
    mkNodeToNodeEncryptionOptions,

    -- * Lenses
    ntneoEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the node-to-node encryption options.
--
-- /See:/ 'mkNodeToNodeEncryptionOptions' smart constructor.
newtype NodeToNodeEncryptionOptions = NodeToNodeEncryptionOptions'
  { -- | Specify true to enable node-to-node encryption.
    enabled :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeToNodeEncryptionOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - Specify true to enable node-to-node encryption.
mkNodeToNodeEncryptionOptions ::
  NodeToNodeEncryptionOptions
mkNodeToNodeEncryptionOptions =
  NodeToNodeEncryptionOptions' {enabled = Lude.Nothing}

-- | Specify true to enable node-to-node encryption.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntneoEnabled :: Lens.Lens' NodeToNodeEncryptionOptions (Lude.Maybe Lude.Bool)
ntneoEnabled = Lens.lens (enabled :: NodeToNodeEncryptionOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: NodeToNodeEncryptionOptions)
{-# DEPRECATED ntneoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromJSON NodeToNodeEncryptionOptions where
  parseJSON =
    Lude.withObject
      "NodeToNodeEncryptionOptions"
      ( \x ->
          NodeToNodeEncryptionOptions' Lude.<$> (x Lude..:? "Enabled")
      )

instance Lude.ToJSON NodeToNodeEncryptionOptions where
  toJSON NodeToNodeEncryptionOptions' {..} =
    Lude.object
      (Lude.catMaybes [("Enabled" Lude..=) Lude.<$> enabled])
