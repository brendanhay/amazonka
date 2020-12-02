{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.HlsIngest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.HlsIngest where

import Network.AWS.Lens
import Network.AWS.MediaPackage.Types.IngestEndpoint
import Network.AWS.Prelude

-- | An HTTP Live Streaming (HLS) ingest resource configuration.
--
-- /See:/ 'hlsIngest' smart constructor.
newtype HlsIngest = HlsIngest'
  { _hiIngestEndpoints ::
      Maybe [IngestEndpoint]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HlsIngest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hiIngestEndpoints' - A list of endpoints to which the source stream should be sent.
hlsIngest ::
  HlsIngest
hlsIngest = HlsIngest' {_hiIngestEndpoints = Nothing}

-- | A list of endpoints to which the source stream should be sent.
hiIngestEndpoints :: Lens' HlsIngest [IngestEndpoint]
hiIngestEndpoints = lens _hiIngestEndpoints (\s a -> s {_hiIngestEndpoints = a}) . _Default . _Coerce

instance FromJSON HlsIngest where
  parseJSON =
    withObject
      "HlsIngest"
      (\x -> HlsIngest' <$> (x .:? "ingestEndpoints" .!= mempty))

instance Hashable HlsIngest

instance NFData HlsIngest
