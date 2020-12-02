{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Endpoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an account-specific API endpoint.
--
-- /See:/ 'endpoint' smart constructor.
newtype Endpoint = Endpoint' {_eURL :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eURL' - URL of endpoint
endpoint ::
  Endpoint
endpoint = Endpoint' {_eURL = Nothing}

-- | URL of endpoint
eURL :: Lens' Endpoint (Maybe Text)
eURL = lens _eURL (\s a -> s {_eURL = a})

instance FromJSON Endpoint where
  parseJSON =
    withObject "Endpoint" (\x -> Endpoint' <$> (x .:? "url"))

instance Hashable Endpoint

instance NFData Endpoint
