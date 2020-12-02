{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMetadataOptionsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMetadataOptionsResponse where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.HTTPTokensState
import Network.AWS.EC2.Types.InstanceMetadataEndpointState
import Network.AWS.EC2.Types.InstanceMetadataOptionsState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata options for the instance.
--
--
--
-- /See:/ 'instanceMetadataOptionsResponse' smart constructor.
data InstanceMetadataOptionsResponse = InstanceMetadataOptionsResponse'
  { _imoState ::
      !( Maybe
           InstanceMetadataOptionsState
       ),
    _imoHTTPEndpoint ::
      !( Maybe
           InstanceMetadataEndpointState
       ),
    _imoHTTPPutResponseHopLimit ::
      !(Maybe Int),
    _imoHTTPTokens ::
      !(Maybe HTTPTokensState)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceMetadataOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imoState' - The state of the metadata option changes. @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection. @applied@ - The metadata options have been successfully applied on the instance.
--
-- * 'imoHTTPEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- * 'imoHTTPPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. Default: 1 Possible values: Integers from 1 to 64
--
-- * 'imoHTTPTokens' - The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ . If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned. If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credential always returns the version 2.0 credentials; the version 1.0 credentials are not available.
instanceMetadataOptionsResponse ::
  InstanceMetadataOptionsResponse
instanceMetadataOptionsResponse =
  InstanceMetadataOptionsResponse'
    { _imoState = Nothing,
      _imoHTTPEndpoint = Nothing,
      _imoHTTPPutResponseHopLimit = Nothing,
      _imoHTTPTokens = Nothing
    }

-- | The state of the metadata option changes. @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection. @applied@ - The metadata options have been successfully applied on the instance.
imoState :: Lens' InstanceMetadataOptionsResponse (Maybe InstanceMetadataOptionsState)
imoState = lens _imoState (\s a -> s {_imoState = a})

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
imoHTTPEndpoint :: Lens' InstanceMetadataOptionsResponse (Maybe InstanceMetadataEndpointState)
imoHTTPEndpoint = lens _imoHTTPEndpoint (\s a -> s {_imoHTTPEndpoint = a})

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. Default: 1 Possible values: Integers from 1 to 64
imoHTTPPutResponseHopLimit :: Lens' InstanceMetadataOptionsResponse (Maybe Int)
imoHTTPPutResponseHopLimit = lens _imoHTTPPutResponseHopLimit (\s a -> s {_imoHTTPPutResponseHopLimit = a})

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ . If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned. If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credential always returns the version 2.0 credentials; the version 1.0 credentials are not available.
imoHTTPTokens :: Lens' InstanceMetadataOptionsResponse (Maybe HTTPTokensState)
imoHTTPTokens = lens _imoHTTPTokens (\s a -> s {_imoHTTPTokens = a})

instance FromXML InstanceMetadataOptionsResponse where
  parseXML x =
    InstanceMetadataOptionsResponse'
      <$> (x .@? "state")
      <*> (x .@? "httpEndpoint")
      <*> (x .@? "httpPutResponseHopLimit")
      <*> (x .@? "httpTokens")

instance Hashable InstanceMetadataOptionsResponse

instance NFData InstanceMetadataOptionsResponse
