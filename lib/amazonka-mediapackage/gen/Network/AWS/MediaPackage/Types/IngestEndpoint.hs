{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.IngestEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.IngestEndpoint
  ( IngestEndpoint (..)
  -- * Smart constructor
  , mkIngestEndpoint
  -- * Lenses
  , ieId
  , iePassword
  , ieUrl
  , ieUsername
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An endpoint for ingesting source content for a Channel.
--
-- /See:/ 'mkIngestEndpoint' smart constructor.
data IngestEndpoint = IngestEndpoint'
  { id :: Core.Maybe Core.Text
    -- ^ The system generated unique identifier for the IngestEndpoint
  , password :: Core.Maybe Core.Text
    -- ^ The system generated password for ingest authentication.
  , url :: Core.Maybe Core.Text
    -- ^ The ingest URL to which the source stream should be sent.
  , username :: Core.Maybe Core.Text
    -- ^ The system generated username for ingest authentication.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IngestEndpoint' value with any optional fields omitted.
mkIngestEndpoint
    :: IngestEndpoint
mkIngestEndpoint
  = IngestEndpoint'{id = Core.Nothing, password = Core.Nothing,
                    url = Core.Nothing, username = Core.Nothing}

-- | The system generated unique identifier for the IngestEndpoint
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieId :: Lens.Lens' IngestEndpoint (Core.Maybe Core.Text)
ieId = Lens.field @"id"
{-# INLINEABLE ieId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The system generated password for ingest authentication.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iePassword :: Lens.Lens' IngestEndpoint (Core.Maybe Core.Text)
iePassword = Lens.field @"password"
{-# INLINEABLE iePassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | The ingest URL to which the source stream should be sent.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieUrl :: Lens.Lens' IngestEndpoint (Core.Maybe Core.Text)
ieUrl = Lens.field @"url"
{-# INLINEABLE ieUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The system generated username for ingest authentication.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieUsername :: Lens.Lens' IngestEndpoint (Core.Maybe Core.Text)
ieUsername = Lens.field @"username"
{-# INLINEABLE ieUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON IngestEndpoint where
        parseJSON
          = Core.withObject "IngestEndpoint" Core.$
              \ x ->
                IngestEndpoint' Core.<$>
                  (x Core..:? "id") Core.<*> x Core..:? "password" Core.<*>
                    x Core..:? "url"
                    Core.<*> x Core..:? "username"
