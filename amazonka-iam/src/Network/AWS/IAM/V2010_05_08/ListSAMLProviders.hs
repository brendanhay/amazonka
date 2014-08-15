{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ListSAMLProviders
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the SAML providers in the account. This operation requires Signature
-- Version 4. https://iam.amazonaws.com/ ?Action=ListSAMLProviders
-- &MaxItems=100 &PathPrefix=/application_abc/ &Version=2010-05-08 &AUTHPARAMS
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Database
-- 2032-05-09T16:27:11Z 2012-05-09T16:27:03Z
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2015-03-11T13:11:02Z 2012-05-09T16:27:11Z
-- fd74fa8d-99f3-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.ListSAMLProviders where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data ListSAMLProviders = ListSAMLProviders
    deriving (Eq, Show, Generic)

makeLenses ''ListSAMLProviders

instance ToQuery ListSAMLProviders where
    toQuery = genericQuery def

data ListSAMLProvidersResponse = ListSAMLProvidersResponse
    { _lsamlpsSAMLProviderList :: [SAMLProviderListEntry]
      -- ^ The list of SAML providers for this account.
    } deriving (Show, Generic)

makeLenses ''ListSAMLProvidersResponse

instance FromXML ListSAMLProvidersResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListSAMLProviders where
    type Sv ListSAMLProviders = IAM
    type Rs ListSAMLProviders = ListSAMLProvidersResponse

    request = post "ListSAMLProviders"
    response _ = xmlResponse
