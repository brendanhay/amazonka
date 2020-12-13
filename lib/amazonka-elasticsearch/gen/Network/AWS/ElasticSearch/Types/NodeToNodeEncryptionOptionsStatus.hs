{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
  ( NodeToNodeEncryptionOptionsStatus (..),

    -- * Smart constructor
    mkNodeToNodeEncryptionOptionsStatus,

    -- * Lenses
    ntneosStatus,
    ntneosOptions,
  )
where

import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status of the node-to-node encryption options for the specified Elasticsearch domain.
--
-- /See:/ 'mkNodeToNodeEncryptionOptionsStatus' smart constructor.
data NodeToNodeEncryptionOptionsStatus = NodeToNodeEncryptionOptionsStatus'
  { -- | Specifies the status of the node-to-node encryption options for the specified Elasticsearch domain.
    status :: OptionStatus,
    -- | Specifies the node-to-node encryption options for the specified Elasticsearch domain.
    options :: NodeToNodeEncryptionOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeToNodeEncryptionOptionsStatus' with the minimum fields required to make a request.
--
-- * 'status' - Specifies the status of the node-to-node encryption options for the specified Elasticsearch domain.
-- * 'options' - Specifies the node-to-node encryption options for the specified Elasticsearch domain.
mkNodeToNodeEncryptionOptionsStatus ::
  -- | 'status'
  OptionStatus ->
  -- | 'options'
  NodeToNodeEncryptionOptions ->
  NodeToNodeEncryptionOptionsStatus
mkNodeToNodeEncryptionOptionsStatus pStatus_ pOptions_ =
  NodeToNodeEncryptionOptionsStatus'
    { status = pStatus_,
      options = pOptions_
    }

-- | Specifies the status of the node-to-node encryption options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntneosStatus :: Lens.Lens' NodeToNodeEncryptionOptionsStatus OptionStatus
ntneosStatus = Lens.lens (status :: NodeToNodeEncryptionOptionsStatus -> OptionStatus) (\s a -> s {status = a} :: NodeToNodeEncryptionOptionsStatus)
{-# DEPRECATED ntneosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the node-to-node encryption options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntneosOptions :: Lens.Lens' NodeToNodeEncryptionOptionsStatus NodeToNodeEncryptionOptions
ntneosOptions = Lens.lens (options :: NodeToNodeEncryptionOptionsStatus -> NodeToNodeEncryptionOptions) (\s a -> s {options = a} :: NodeToNodeEncryptionOptionsStatus)
{-# DEPRECATED ntneosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromJSON NodeToNodeEncryptionOptionsStatus where
  parseJSON =
    Lude.withObject
      "NodeToNodeEncryptionOptionsStatus"
      ( \x ->
          NodeToNodeEncryptionOptionsStatus'
            Lude.<$> (x Lude..: "Status") Lude.<*> (x Lude..: "Options")
      )
