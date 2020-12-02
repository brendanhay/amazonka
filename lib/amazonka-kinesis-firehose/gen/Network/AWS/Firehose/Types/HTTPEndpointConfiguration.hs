{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration of the HTTP endpoint to which Kinesis Firehose delivers data.
--
--
--
-- /See:/ 'hTTPEndpointConfiguration' smart constructor.
data HTTPEndpointConfiguration = HTTPEndpointConfiguration'
  { _httpecName ::
      !(Maybe Text),
    _httpecAccessKey ::
      !(Maybe (Sensitive Text)),
    _httpecURL :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPEndpointConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpecName' - The name of the HTTP endpoint selected as the destination.
--
-- * 'httpecAccessKey' - The access key required for Kinesis Firehose to authenticate with the HTTP endpoint selected as the destination.
--
-- * 'httpecURL' - The URL of the HTTP endpoint selected as the destination.
hTTPEndpointConfiguration ::
  -- | 'httpecURL'
  Text ->
  HTTPEndpointConfiguration
hTTPEndpointConfiguration pURL_ =
  HTTPEndpointConfiguration'
    { _httpecName = Nothing,
      _httpecAccessKey = Nothing,
      _httpecURL = _Sensitive # pURL_
    }

-- | The name of the HTTP endpoint selected as the destination.
httpecName :: Lens' HTTPEndpointConfiguration (Maybe Text)
httpecName = lens _httpecName (\s a -> s {_httpecName = a})

-- | The access key required for Kinesis Firehose to authenticate with the HTTP endpoint selected as the destination.
httpecAccessKey :: Lens' HTTPEndpointConfiguration (Maybe Text)
httpecAccessKey = lens _httpecAccessKey (\s a -> s {_httpecAccessKey = a}) . mapping _Sensitive

-- | The URL of the HTTP endpoint selected as the destination.
httpecURL :: Lens' HTTPEndpointConfiguration Text
httpecURL = lens _httpecURL (\s a -> s {_httpecURL = a}) . _Sensitive

instance Hashable HTTPEndpointConfiguration

instance NFData HTTPEndpointConfiguration

instance ToJSON HTTPEndpointConfiguration where
  toJSON HTTPEndpointConfiguration' {..} =
    object
      ( catMaybes
          [ ("Name" .=) <$> _httpecName,
            ("AccessKey" .=) <$> _httpecAccessKey,
            Just ("Url" .= _httpecURL)
          ]
      )
