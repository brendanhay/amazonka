{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateHTTPTokensState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--
-- /See:/ 'launchTemplateInstanceMetadataOptions' smart constructor.
data LaunchTemplateInstanceMetadataOptions = LaunchTemplateInstanceMetadataOptions'
  { _ltimoState ::
      !( Maybe
           LaunchTemplateInstanceMetadataOptionsState
       ),
    _ltimoHTTPEndpoint ::
      !( Maybe
           LaunchTemplateInstanceMetadataEndpointState
       ),
    _ltimoHTTPPutResponseHopLimit ::
      !(Maybe Int),
    _ltimoHTTPTokens ::
      !( Maybe
           LaunchTemplateHTTPTokensState
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateInstanceMetadataOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltimoState' - The state of the metadata option changes. @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection. @applied@ - The metadata options have been successfully applied on the instance.
--
-- * 'ltimoHTTPEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- * 'ltimoHTTPPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. Default: 1 Possible values: Integers from 1 to 64
--
-- * 'ltimoHTTPTokens' - The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ . If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned. If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
launchTemplateInstanceMetadataOptions ::
  LaunchTemplateInstanceMetadataOptions
launchTemplateInstanceMetadataOptions =
  LaunchTemplateInstanceMetadataOptions'
    { _ltimoState = Nothing,
      _ltimoHTTPEndpoint = Nothing,
      _ltimoHTTPPutResponseHopLimit = Nothing,
      _ltimoHTTPTokens = Nothing
    }

-- | The state of the metadata option changes. @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection. @applied@ - The metadata options have been successfully applied on the instance.
ltimoState :: Lens' LaunchTemplateInstanceMetadataOptions (Maybe LaunchTemplateInstanceMetadataOptionsState)
ltimoState = lens _ltimoState (\s a -> s {_ltimoState = a})

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
ltimoHTTPEndpoint :: Lens' LaunchTemplateInstanceMetadataOptions (Maybe LaunchTemplateInstanceMetadataEndpointState)
ltimoHTTPEndpoint = lens _ltimoHTTPEndpoint (\s a -> s {_ltimoHTTPEndpoint = a})

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. Default: 1 Possible values: Integers from 1 to 64
ltimoHTTPPutResponseHopLimit :: Lens' LaunchTemplateInstanceMetadataOptions (Maybe Int)
ltimoHTTPPutResponseHopLimit = lens _ltimoHTTPPutResponseHopLimit (\s a -> s {_ltimoHTTPPutResponseHopLimit = a})

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ . If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned. If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
ltimoHTTPTokens :: Lens' LaunchTemplateInstanceMetadataOptions (Maybe LaunchTemplateHTTPTokensState)
ltimoHTTPTokens = lens _ltimoHTTPTokens (\s a -> s {_ltimoHTTPTokens = a})

instance FromXML LaunchTemplateInstanceMetadataOptions where
  parseXML x =
    LaunchTemplateInstanceMetadataOptions'
      <$> (x .@? "state")
      <*> (x .@? "httpEndpoint")
      <*> (x .@? "httpPutResponseHopLimit")
      <*> (x .@? "httpTokens")

instance Hashable LaunchTemplateInstanceMetadataOptions

instance NFData LaunchTemplateInstanceMetadataOptions
