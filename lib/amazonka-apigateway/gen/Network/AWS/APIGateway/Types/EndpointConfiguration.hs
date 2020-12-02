{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.EndpointConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.EndpointConfiguration where

import Network.AWS.APIGateway.Types.EndpointType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The endpoint configuration to indicate the types of endpoints an API ('RestApi' ) or its custom domain name ('DomainName' ) has.
--
--
--
-- /See:/ 'endpointConfiguration' smart constructor.
data EndpointConfiguration = EndpointConfiguration'
  { _ecTypes ::
      !(Maybe [EndpointType]),
    _ecVpcEndpointIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecTypes' - A list of endpoint types of an API ('RestApi' ) or its custom domain name ('DomainName' ). For an edge-optimized API and its custom domain name, the endpoint type is @"EDGE"@ . For a regional API and its custom domain name, the endpoint type is @REGIONAL@ . For a private API, the endpoint type is @PRIVATE@ .
--
-- * 'ecVpcEndpointIds' - A list of VpcEndpointIds of an API ('RestApi' ) against which to create Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
endpointConfiguration ::
  EndpointConfiguration
endpointConfiguration =
  EndpointConfiguration'
    { _ecTypes = Nothing,
      _ecVpcEndpointIds = Nothing
    }

-- | A list of endpoint types of an API ('RestApi' ) or its custom domain name ('DomainName' ). For an edge-optimized API and its custom domain name, the endpoint type is @"EDGE"@ . For a regional API and its custom domain name, the endpoint type is @REGIONAL@ . For a private API, the endpoint type is @PRIVATE@ .
ecTypes :: Lens' EndpointConfiguration [EndpointType]
ecTypes = lens _ecTypes (\s a -> s {_ecTypes = a}) . _Default . _Coerce

-- | A list of VpcEndpointIds of an API ('RestApi' ) against which to create Route53 ALIASes. It is only supported for @PRIVATE@ endpoint type.
ecVpcEndpointIds :: Lens' EndpointConfiguration [Text]
ecVpcEndpointIds = lens _ecVpcEndpointIds (\s a -> s {_ecVpcEndpointIds = a}) . _Default . _Coerce

instance FromJSON EndpointConfiguration where
  parseJSON =
    withObject
      "EndpointConfiguration"
      ( \x ->
          EndpointConfiguration'
            <$> (x .:? "types" .!= mempty) <*> (x .:? "vpcEndpointIds" .!= mempty)
      )

instance Hashable EndpointConfiguration

instance NFData EndpointConfiguration

instance ToJSON EndpointConfiguration where
  toJSON EndpointConfiguration' {..} =
    object
      ( catMaybes
          [ ("types" .=) <$> _ecTypes,
            ("vpcEndpointIds" .=) <$> _ecVpcEndpointIds
          ]
      )
