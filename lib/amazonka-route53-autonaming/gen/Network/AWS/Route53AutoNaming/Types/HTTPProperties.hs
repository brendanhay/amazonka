{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HTTPProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HTTPProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains the name of an HTTP namespace.
--
--
--
-- /See:/ 'hTTPProperties' smart constructor.
newtype HTTPProperties = HTTPProperties'
  { _httppHTTPName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httppHTTPName' - The name of an HTTP namespace.
hTTPProperties ::
  HTTPProperties
hTTPProperties = HTTPProperties' {_httppHTTPName = Nothing}

-- | The name of an HTTP namespace.
httppHTTPName :: Lens' HTTPProperties (Maybe Text)
httppHTTPName = lens _httppHTTPName (\s a -> s {_httppHTTPName = a})

instance FromJSON HTTPProperties where
  parseJSON =
    withObject
      "HTTPProperties"
      (\x -> HTTPProperties' <$> (x .:? "HttpName"))

instance Hashable HTTPProperties

instance NFData HTTPProperties
