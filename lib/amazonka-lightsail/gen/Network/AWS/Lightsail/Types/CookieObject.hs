{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CookieObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CookieObject where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.ForwardValues
import Network.AWS.Prelude

-- | Describes whether an Amazon Lightsail content delivery network (CDN) distribution forwards cookies to the origin and, if so, which ones.
--
--
-- For the cookies that you specify, your distribution caches separate versions of the specified content based on the cookie values in viewer requests.
--
--
-- /See:/ 'cookieObject' smart constructor.
data CookieObject = CookieObject'
  { _coCookiesAllowList ::
      !(Maybe [Text]),
    _coOption :: !(Maybe ForwardValues)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CookieObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCookiesAllowList' - The specific cookies to forward to your distribution's origin.
--
-- * 'coOption' - Specifies which cookies to forward to the distribution's origin for a cache behavior: @all@ , @none@ , or @allow-list@ to forward only the cookies specified in the @cookiesAllowList@ parameter.
cookieObject ::
  CookieObject
cookieObject =
  CookieObject' {_coCookiesAllowList = Nothing, _coOption = Nothing}

-- | The specific cookies to forward to your distribution's origin.
coCookiesAllowList :: Lens' CookieObject [Text]
coCookiesAllowList = lens _coCookiesAllowList (\s a -> s {_coCookiesAllowList = a}) . _Default . _Coerce

-- | Specifies which cookies to forward to the distribution's origin for a cache behavior: @all@ , @none@ , or @allow-list@ to forward only the cookies specified in the @cookiesAllowList@ parameter.
coOption :: Lens' CookieObject (Maybe ForwardValues)
coOption = lens _coOption (\s a -> s {_coOption = a})

instance FromJSON CookieObject where
  parseJSON =
    withObject
      "CookieObject"
      ( \x ->
          CookieObject'
            <$> (x .:? "cookiesAllowList" .!= mempty) <*> (x .:? "option")
      )

instance Hashable CookieObject

instance NFData CookieObject

instance ToJSON CookieObject where
  toJSON CookieObject' {..} =
    object
      ( catMaybes
          [ ("cookiesAllowList" .=) <$> _coCookiesAllowList,
            ("option" .=) <$> _coOption
          ]
      )
