{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.IngestEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.IngestEndpoint
  ( IngestEndpoint (..),

    -- * Smart constructor
    mkIngestEndpoint,

    -- * Lenses
    ieURL,
    ieUsername,
    iePassword,
    ieId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An endpoint for ingesting source content for a Channel.
--
-- /See:/ 'mkIngestEndpoint' smart constructor.
data IngestEndpoint = IngestEndpoint'
  { url :: Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    password :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IngestEndpoint' with the minimum fields required to make a request.
--
-- * 'id' - The system generated unique identifier for the IngestEndpoint
-- * 'password' - The system generated password for ingest authentication.
-- * 'url' - The ingest URL to which the source stream should be sent.
-- * 'username' - The system generated username for ingest authentication.
mkIngestEndpoint ::
  IngestEndpoint
mkIngestEndpoint =
  IngestEndpoint'
    { url = Lude.Nothing,
      username = Lude.Nothing,
      password = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The ingest URL to which the source stream should be sent.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieURL :: Lens.Lens' IngestEndpoint (Lude.Maybe Lude.Text)
ieURL = Lens.lens (url :: IngestEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: IngestEndpoint)
{-# DEPRECATED ieURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The system generated username for ingest authentication.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieUsername :: Lens.Lens' IngestEndpoint (Lude.Maybe Lude.Text)
ieUsername = Lens.lens (username :: IngestEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: IngestEndpoint)
{-# DEPRECATED ieUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The system generated password for ingest authentication.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iePassword :: Lens.Lens' IngestEndpoint (Lude.Maybe Lude.Text)
iePassword = Lens.lens (password :: IngestEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: IngestEndpoint)
{-# DEPRECATED iePassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The system generated unique identifier for the IngestEndpoint
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ieId :: Lens.Lens' IngestEndpoint (Lude.Maybe Lude.Text)
ieId = Lens.lens (id :: IngestEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: IngestEndpoint)
{-# DEPRECATED ieId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON IngestEndpoint where
  parseJSON =
    Lude.withObject
      "IngestEndpoint"
      ( \x ->
          IngestEndpoint'
            Lude.<$> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "username")
            Lude.<*> (x Lude..:? "password")
            Lude.<*> (x Lude..:? "id")
      )
