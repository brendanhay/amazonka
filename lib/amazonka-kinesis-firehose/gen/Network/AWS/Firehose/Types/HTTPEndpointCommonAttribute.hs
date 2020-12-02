{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointCommonAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointCommonAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the metadata that's delivered to the specified HTTP endpoint destination.
--
--
--
-- /See:/ 'hTTPEndpointCommonAttribute' smart constructor.
data HTTPEndpointCommonAttribute = HTTPEndpointCommonAttribute'
  { _httpecaAttributeName ::
      !(Sensitive Text),
    _httpecaAttributeValue ::
      !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPEndpointCommonAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpecaAttributeName' - The name of the HTTP endpoint common attribute.
--
-- * 'httpecaAttributeValue' - The value of the HTTP endpoint common attribute.
hTTPEndpointCommonAttribute ::
  -- | 'httpecaAttributeName'
  Text ->
  -- | 'httpecaAttributeValue'
  Text ->
  HTTPEndpointCommonAttribute
hTTPEndpointCommonAttribute pAttributeName_ pAttributeValue_ =
  HTTPEndpointCommonAttribute'
    { _httpecaAttributeName =
        _Sensitive # pAttributeName_,
      _httpecaAttributeValue = _Sensitive # pAttributeValue_
    }

-- | The name of the HTTP endpoint common attribute.
httpecaAttributeName :: Lens' HTTPEndpointCommonAttribute Text
httpecaAttributeName = lens _httpecaAttributeName (\s a -> s {_httpecaAttributeName = a}) . _Sensitive

-- | The value of the HTTP endpoint common attribute.
httpecaAttributeValue :: Lens' HTTPEndpointCommonAttribute Text
httpecaAttributeValue = lens _httpecaAttributeValue (\s a -> s {_httpecaAttributeValue = a}) . _Sensitive

instance FromJSON HTTPEndpointCommonAttribute where
  parseJSON =
    withObject
      "HTTPEndpointCommonAttribute"
      ( \x ->
          HTTPEndpointCommonAttribute'
            <$> (x .: "AttributeName") <*> (x .: "AttributeValue")
      )

instance Hashable HTTPEndpointCommonAttribute

instance NFData HTTPEndpointCommonAttribute

instance ToJSON HTTPEndpointCommonAttribute where
  toJSON HTTPEndpointCommonAttribute' {..} =
    object
      ( catMaybes
          [ Just ("AttributeName" .= _httpecaAttributeName),
            Just ("AttributeValue" .= _httpecaAttributeValue)
          ]
      )
