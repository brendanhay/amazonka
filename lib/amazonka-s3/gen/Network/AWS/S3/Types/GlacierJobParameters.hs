{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.GlacierJobParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.GlacierJobParameters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Tier

-- | Container for S3 Glacier job parameters.
--
--
--
-- /See:/ 'glacierJobParameters' smart constructor.
newtype GlacierJobParameters = GlacierJobParameters'
  { _gjpTier ::
      Tier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlacierJobParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gjpTier' - Retrieval tier at which the restore will be processed.
glacierJobParameters ::
  -- | 'gjpTier'
  Tier ->
  GlacierJobParameters
glacierJobParameters pTier_ =
  GlacierJobParameters' {_gjpTier = pTier_}

-- | Retrieval tier at which the restore will be processed.
gjpTier :: Lens' GlacierJobParameters Tier
gjpTier = lens _gjpTier (\s a -> s {_gjpTier = a})

instance Hashable GlacierJobParameters

instance NFData GlacierJobParameters

instance ToXML GlacierJobParameters where
  toXML GlacierJobParameters' {..} = mconcat ["Tier" @= _gjpTier]
