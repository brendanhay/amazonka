{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateHTTPTokensState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--
-- /See:/ 'launchTemplateInstanceMetadataOptionsRequest' smart constructor.
data LaunchTemplateInstanceMetadataOptionsRequest = LaunchTemplateInstanceMetadataOptionsRequest'
  { _ltimorHTTPEndpoint ::
      !( Maybe
           LaunchTemplateInstanceMetadataEndpointState
       ),
    _ltimorHTTPPutResponseHopLimit ::
      !( Maybe
           Int
       ),
    _ltimorHTTPTokens ::
      !( Maybe
           LaunchTemplateHTTPTokensState
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'LaunchTemplateInstanceMetadataOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltimorHTTPEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- * 'ltimorHTTPPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. Default: 1 Possible values: Integers from 1 to 64
--
-- * 'ltimorHTTPTokens' - The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ . If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned. If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
launchTemplateInstanceMetadataOptionsRequest ::
  LaunchTemplateInstanceMetadataOptionsRequest
launchTemplateInstanceMetadataOptionsRequest =
  LaunchTemplateInstanceMetadataOptionsRequest'
    { _ltimorHTTPEndpoint =
        Nothing,
      _ltimorHTTPPutResponseHopLimit = Nothing,
      _ltimorHTTPTokens = Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
ltimorHTTPEndpoint :: Lens' LaunchTemplateInstanceMetadataOptionsRequest (Maybe LaunchTemplateInstanceMetadataEndpointState)
ltimorHTTPEndpoint = lens _ltimorHTTPEndpoint (\s a -> s {_ltimorHTTPEndpoint = a})

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. Default: 1 Possible values: Integers from 1 to 64
ltimorHTTPPutResponseHopLimit :: Lens' LaunchTemplateInstanceMetadataOptionsRequest (Maybe Int)
ltimorHTTPPutResponseHopLimit = lens _ltimorHTTPPutResponseHopLimit (\s a -> s {_ltimorHTTPPutResponseHopLimit = a})

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ . If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned. If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
ltimorHTTPTokens :: Lens' LaunchTemplateInstanceMetadataOptionsRequest (Maybe LaunchTemplateHTTPTokensState)
ltimorHTTPTokens = lens _ltimorHTTPTokens (\s a -> s {_ltimorHTTPTokens = a})

instance Hashable LaunchTemplateInstanceMetadataOptionsRequest

instance NFData LaunchTemplateInstanceMetadataOptionsRequest

instance ToQuery LaunchTemplateInstanceMetadataOptionsRequest where
  toQuery LaunchTemplateInstanceMetadataOptionsRequest' {..} =
    mconcat
      [ "HttpEndpoint" =: _ltimorHTTPEndpoint,
        "HttpPutResponseHopLimit" =: _ltimorHTTPPutResponseHopLimit,
        "HttpTokens" =: _ltimorHTTPTokens
      ]
