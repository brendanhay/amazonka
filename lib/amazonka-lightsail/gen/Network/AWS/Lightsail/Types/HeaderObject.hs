{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.HeaderObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.HeaderObject where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.ForwardValues
import Network.AWS.Lightsail.Types.HeaderEnum
import Network.AWS.Prelude

-- | Describes the request headers that a Lightsail distribution bases caching on.
--
--
-- For the headers that you specify, your distribution caches separate versions of the specified content based on the header values in viewer requests. For example, suppose viewer requests for @logo.jpg@ contain a custom @product@ header that has a value of either @acme@ or @apex@ , and you configure your distribution to cache your content based on values in the @product@ header. Your distribution forwards the @product@ header to the origin and caches the response from the origin once for each header value.
--
--
-- /See:/ 'headerObject' smart constructor.
data HeaderObject = HeaderObject'
  { _hoHeadersAllowList ::
      !(Maybe [HeaderEnum]),
    _hoOption :: !(Maybe ForwardValues)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HeaderObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hoHeadersAllowList' - The specific headers to forward to your distribution's origin.
--
-- * 'hoOption' - The headers that you want your distribution to forward to your origin and base caching on. You can configure your distribution to do one of the following:     * __@all@ __ - Forward all headers to your origin.     * __@none@ __ - Forward only the default headers.     * __@allow-list@ __ - Forward only the headers you specify using the @headersAllowList@ parameter.
headerObject ::
  HeaderObject
headerObject =
  HeaderObject' {_hoHeadersAllowList = Nothing, _hoOption = Nothing}

-- | The specific headers to forward to your distribution's origin.
hoHeadersAllowList :: Lens' HeaderObject [HeaderEnum]
hoHeadersAllowList = lens _hoHeadersAllowList (\s a -> s {_hoHeadersAllowList = a}) . _Default . _Coerce

-- | The headers that you want your distribution to forward to your origin and base caching on. You can configure your distribution to do one of the following:     * __@all@ __ - Forward all headers to your origin.     * __@none@ __ - Forward only the default headers.     * __@allow-list@ __ - Forward only the headers you specify using the @headersAllowList@ parameter.
hoOption :: Lens' HeaderObject (Maybe ForwardValues)
hoOption = lens _hoOption (\s a -> s {_hoOption = a})

instance FromJSON HeaderObject where
  parseJSON =
    withObject
      "HeaderObject"
      ( \x ->
          HeaderObject'
            <$> (x .:? "headersAllowList" .!= mempty) <*> (x .:? "option")
      )

instance Hashable HeaderObject

instance NFData HeaderObject

instance ToJSON HeaderObject where
  toJSON HeaderObject' {..} =
    object
      ( catMaybes
          [ ("headersAllowList" .=) <$> _hoHeadersAllowList,
            ("option" .=) <$> _hoOption
          ]
      )
