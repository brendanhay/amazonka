{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig where

import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
import Network.AWS.CloudFront.Types.QueryStringNames
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that determines whether any URL query strings in viewer requests (and if so, which query strings) are included in requests that CloudFront sends to the origin.
--
--
--
-- /See:/ 'originRequestPolicyQueryStringsConfig' smart constructor.
data OriginRequestPolicyQueryStringsConfig = OriginRequestPolicyQueryStringsConfig'
  { _orpqscQueryStrings ::
      !( Maybe
           QueryStringNames
       ),
    _orpqscQueryStringBehavior ::
      !OriginRequestPolicyQueryStringBehavior
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginRequestPolicyQueryStringsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orpqscQueryStrings' - Contains a list of the query strings in viewer requests that are included in requests that CloudFront sends to the origin.
--
-- * 'orpqscQueryStringBehavior' - Determines whether any URL query strings in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – Query strings in viewer requests are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any query strings that are listed in a @CachePolicy@ /are/ included in origin requests.     * @whitelist@ – The query strings in viewer requests that are listed in the @QueryStringNames@ type are included in requests that CloudFront sends to the origin.     * @all@ – All query strings in viewer requests are included in requests that CloudFront sends to the origin.
originRequestPolicyQueryStringsConfig ::
  -- | 'orpqscQueryStringBehavior'
  OriginRequestPolicyQueryStringBehavior ->
  OriginRequestPolicyQueryStringsConfig
originRequestPolicyQueryStringsConfig pQueryStringBehavior_ =
  OriginRequestPolicyQueryStringsConfig'
    { _orpqscQueryStrings =
        Nothing,
      _orpqscQueryStringBehavior = pQueryStringBehavior_
    }

-- | Contains a list of the query strings in viewer requests that are included in requests that CloudFront sends to the origin.
orpqscQueryStrings :: Lens' OriginRequestPolicyQueryStringsConfig (Maybe QueryStringNames)
orpqscQueryStrings = lens _orpqscQueryStrings (\s a -> s {_orpqscQueryStrings = a})

-- | Determines whether any URL query strings in viewer requests are included in requests that CloudFront sends to the origin. Valid values are:     * @none@ – Query strings in viewer requests are not included in requests that CloudFront sends to the origin. Even when this field is set to @none@ , any query strings that are listed in a @CachePolicy@ /are/ included in origin requests.     * @whitelist@ – The query strings in viewer requests that are listed in the @QueryStringNames@ type are included in requests that CloudFront sends to the origin.     * @all@ – All query strings in viewer requests are included in requests that CloudFront sends to the origin.
orpqscQueryStringBehavior :: Lens' OriginRequestPolicyQueryStringsConfig OriginRequestPolicyQueryStringBehavior
orpqscQueryStringBehavior = lens _orpqscQueryStringBehavior (\s a -> s {_orpqscQueryStringBehavior = a})

instance FromXML OriginRequestPolicyQueryStringsConfig where
  parseXML x =
    OriginRequestPolicyQueryStringsConfig'
      <$> (x .@? "QueryStrings") <*> (x .@ "QueryStringBehavior")

instance Hashable OriginRequestPolicyQueryStringsConfig

instance NFData OriginRequestPolicyQueryStringsConfig

instance ToXML OriginRequestPolicyQueryStringsConfig where
  toXML OriginRequestPolicyQueryStringsConfig' {..} =
    mconcat
      [ "QueryStrings" @= _orpqscQueryStrings,
        "QueryStringBehavior" @= _orpqscQueryStringBehavior
      ]
