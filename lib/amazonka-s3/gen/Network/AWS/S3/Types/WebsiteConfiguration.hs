{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.WebsiteConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.WebsiteConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ErrorDocument
import Network.AWS.S3.Types.IndexDocument
import Network.AWS.S3.Types.RedirectAllRequestsTo
import Network.AWS.S3.Types.RoutingRule

-- | Specifies website configuration parameters for an Amazon S3 bucket.
--
--
--
-- /See:/ 'websiteConfiguration' smart constructor.
data WebsiteConfiguration = WebsiteConfiguration'
  { _wcRedirectAllRequestsTo ::
      !(Maybe RedirectAllRequestsTo),
    _wcErrorDocument :: !(Maybe ErrorDocument),
    _wcIndexDocument :: !(Maybe IndexDocument),
    _wcRoutingRules :: !(Maybe [RoutingRule])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WebsiteConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wcRedirectAllRequestsTo' - The redirect behavior for every request to this bucket's website endpoint. /Important:/ If you specify this property, you can't specify any other property.
--
-- * 'wcErrorDocument' - The name of the error document for the website.
--
-- * 'wcIndexDocument' - The name of the index document for the website.
--
-- * 'wcRoutingRules' - Rules that define when a redirect is applied and the redirect behavior.
websiteConfiguration ::
  WebsiteConfiguration
websiteConfiguration =
  WebsiteConfiguration'
    { _wcRedirectAllRequestsTo = Nothing,
      _wcErrorDocument = Nothing,
      _wcIndexDocument = Nothing,
      _wcRoutingRules = Nothing
    }

-- | The redirect behavior for every request to this bucket's website endpoint. /Important:/ If you specify this property, you can't specify any other property.
wcRedirectAllRequestsTo :: Lens' WebsiteConfiguration (Maybe RedirectAllRequestsTo)
wcRedirectAllRequestsTo = lens _wcRedirectAllRequestsTo (\s a -> s {_wcRedirectAllRequestsTo = a})

-- | The name of the error document for the website.
wcErrorDocument :: Lens' WebsiteConfiguration (Maybe ErrorDocument)
wcErrorDocument = lens _wcErrorDocument (\s a -> s {_wcErrorDocument = a})

-- | The name of the index document for the website.
wcIndexDocument :: Lens' WebsiteConfiguration (Maybe IndexDocument)
wcIndexDocument = lens _wcIndexDocument (\s a -> s {_wcIndexDocument = a})

-- | Rules that define when a redirect is applied and the redirect behavior.
wcRoutingRules :: Lens' WebsiteConfiguration [RoutingRule]
wcRoutingRules = lens _wcRoutingRules (\s a -> s {_wcRoutingRules = a}) . _Default . _Coerce

instance Hashable WebsiteConfiguration

instance NFData WebsiteConfiguration

instance ToXML WebsiteConfiguration where
  toXML WebsiteConfiguration' {..} =
    mconcat
      [ "RedirectAllRequestsTo" @= _wcRedirectAllRequestsTo,
        "ErrorDocument" @= _wcErrorDocument,
        "IndexDocument" @= _wcIndexDocument,
        "RoutingRules"
          @= toXML (toXMLList "RoutingRule" <$> _wcRoutingRules)
      ]
