{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsIngest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsIngest
  ( HlsIngest (..),

    -- * Smart constructor
    mkHlsIngest,

    -- * Lenses
    hiIngestEndpoints,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.IngestEndpoint
import qualified Network.AWS.Prelude as Lude

-- | An HTTP Live Streaming (HLS) ingest resource configuration.
--
-- /See:/ 'mkHlsIngest' smart constructor.
newtype HlsIngest = HlsIngest'
  { -- | A list of endpoints to which the source stream should be sent.
    ingestEndpoints :: Lude.Maybe [IngestEndpoint]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsIngest' with the minimum fields required to make a request.
--
-- * 'ingestEndpoints' - A list of endpoints to which the source stream should be sent.
mkHlsIngest ::
  HlsIngest
mkHlsIngest = HlsIngest' {ingestEndpoints = Lude.Nothing}

-- | A list of endpoints to which the source stream should be sent.
--
-- /Note:/ Consider using 'ingestEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hiIngestEndpoints :: Lens.Lens' HlsIngest (Lude.Maybe [IngestEndpoint])
hiIngestEndpoints = Lens.lens (ingestEndpoints :: HlsIngest -> Lude.Maybe [IngestEndpoint]) (\s a -> s {ingestEndpoints = a} :: HlsIngest)
{-# DEPRECATED hiIngestEndpoints "Use generic-lens or generic-optics with 'ingestEndpoints' instead." #-}

instance Lude.FromJSON HlsIngest where
  parseJSON =
    Lude.withObject
      "HlsIngest"
      ( \x ->
          HlsIngest'
            Lude.<$> (x Lude..:? "ingestEndpoints" Lude..!= Lude.mempty)
      )
