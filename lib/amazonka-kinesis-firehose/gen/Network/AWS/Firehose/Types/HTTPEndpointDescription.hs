{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the HTTP endpoint selected as the destination.
--
--
--
-- /See:/ 'hTTPEndpointDescription' smart constructor.
data HTTPEndpointDescription = HTTPEndpointDescription'
  { _httpedURL ::
      !(Maybe (Sensitive Text)),
    _httpedName :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPEndpointDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpedURL' - The URL of the HTTP endpoint selected as the destination.
--
-- * 'httpedName' - The name of the HTTP endpoint selected as the destination.
hTTPEndpointDescription ::
  HTTPEndpointDescription
hTTPEndpointDescription =
  HTTPEndpointDescription'
    { _httpedURL = Nothing,
      _httpedName = Nothing
    }

-- | The URL of the HTTP endpoint selected as the destination.
httpedURL :: Lens' HTTPEndpointDescription (Maybe Text)
httpedURL = lens _httpedURL (\s a -> s {_httpedURL = a}) . mapping _Sensitive

-- | The name of the HTTP endpoint selected as the destination.
httpedName :: Lens' HTTPEndpointDescription (Maybe Text)
httpedName = lens _httpedName (\s a -> s {_httpedName = a})

instance FromJSON HTTPEndpointDescription where
  parseJSON =
    withObject
      "HTTPEndpointDescription"
      ( \x ->
          HTTPEndpointDescription' <$> (x .:? "Url") <*> (x .:? "Name")
      )

instance Hashable HTTPEndpointDescription

instance NFData HTTPEndpointDescription
