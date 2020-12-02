{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceMetadataOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceMetadataOptions where

import Network.AWS.AutoScaling.Types.InstanceMetadataEndpointState
import Network.AWS.AutoScaling.Types.InstanceMetadataHTTPTokensState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
--
--
-- /See:/ 'instanceMetadataOptions' smart constructor.
data InstanceMetadataOptions = InstanceMetadataOptions'
  { _imoHTTPEndpoint ::
      !(Maybe InstanceMetadataEndpointState),
    _imoHTTPPutResponseHopLimit :: !(Maybe Nat),
    _imoHTTPTokens ::
      !(Maybe InstanceMetadataHTTPTokensState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceMetadataOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imoHTTPEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- * 'imoHTTPPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. Default: 1 Possible values: Integers from 1 to 64
--
-- * 'imoHTTPTokens' - The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ . If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned. If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
instanceMetadataOptions ::
  InstanceMetadataOptions
instanceMetadataOptions =
  InstanceMetadataOptions'
    { _imoHTTPEndpoint = Nothing,
      _imoHTTPPutResponseHopLimit = Nothing,
      _imoHTTPTokens = Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
imoHTTPEndpoint :: Lens' InstanceMetadataOptions (Maybe InstanceMetadataEndpointState)
imoHTTPEndpoint = lens _imoHTTPEndpoint (\s a -> s {_imoHTTPEndpoint = a})

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. Default: 1 Possible values: Integers from 1 to 64
imoHTTPPutResponseHopLimit :: Lens' InstanceMetadataOptions (Maybe Natural)
imoHTTPPutResponseHopLimit = lens _imoHTTPPutResponseHopLimit (\s a -> s {_imoHTTPPutResponseHopLimit = a}) . mapping _Nat

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ . If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned. If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
imoHTTPTokens :: Lens' InstanceMetadataOptions (Maybe InstanceMetadataHTTPTokensState)
imoHTTPTokens = lens _imoHTTPTokens (\s a -> s {_imoHTTPTokens = a})

instance FromXML InstanceMetadataOptions where
  parseXML x =
    InstanceMetadataOptions'
      <$> (x .@? "HttpEndpoint")
      <*> (x .@? "HttpPutResponseHopLimit")
      <*> (x .@? "HttpTokens")

instance Hashable InstanceMetadataOptions

instance NFData InstanceMetadataOptions

instance ToQuery InstanceMetadataOptions where
  toQuery InstanceMetadataOptions' {..} =
    mconcat
      [ "HttpEndpoint" =: _imoHTTPEndpoint,
        "HttpPutResponseHopLimit" =: _imoHTTPPutResponseHopLimit,
        "HttpTokens" =: _imoHTTPTokens
      ]
