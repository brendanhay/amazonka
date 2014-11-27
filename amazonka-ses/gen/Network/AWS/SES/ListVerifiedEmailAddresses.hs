{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.ListVerifiedEmailAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list containing all of the email addresses that have been verified.
--
-- The ListVerifiedEmailAddresses action is deprecated as of the May 15, 2012
-- release of Domain Verification. The ListIdentities action is now preferred. This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_ListVerifiedEmailAddresses.html>
module Network.AWS.SES.ListVerifiedEmailAddresses
    (
    -- * Request
      ListVerifiedEmailAddresses
    -- ** Request constructor
    , listVerifiedEmailAddresses

    -- * Response
    , ListVerifiedEmailAddressesResponse
    -- ** Response constructor
    , listVerifiedEmailAddressesResponse
    -- ** Response lenses
    , lvearVerifiedEmailAddresses
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

data ListVerifiedEmailAddresses = ListVerifiedEmailAddresses
    deriving (Eq, Ord, Show, Generic)

-- | 'ListVerifiedEmailAddresses' constructor.
listVerifiedEmailAddresses :: ListVerifiedEmailAddresses
listVerifiedEmailAddresses = ListVerifiedEmailAddresses

newtype ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse
    { _lvearVerifiedEmailAddresses :: List "ToAddresses" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ListVerifiedEmailAddressesResponse where
    type Item ListVerifiedEmailAddressesResponse = Text

    fromList = ListVerifiedEmailAddressesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _lvearVerifiedEmailAddresses

-- | 'ListVerifiedEmailAddressesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvearVerifiedEmailAddresses' @::@ ['Text']
--
listVerifiedEmailAddressesResponse :: ListVerifiedEmailAddressesResponse
listVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse
    { _lvearVerifiedEmailAddresses = mempty
    }

-- | A list of email addresses that have been verified.
lvearVerifiedEmailAddresses :: Lens' ListVerifiedEmailAddressesResponse [Text]
lvearVerifiedEmailAddresses =
    lens _lvearVerifiedEmailAddresses
        (\s a -> s { _lvearVerifiedEmailAddresses = a })
            . _List

instance ToPath ListVerifiedEmailAddresses where
    toPath = const "/"

instance ToQuery ListVerifiedEmailAddresses where
    toQuery = const mempty

instance ToHeaders ListVerifiedEmailAddresses

instance AWSRequest ListVerifiedEmailAddresses where
    type Sv ListVerifiedEmailAddresses = SES
    type Rs ListVerifiedEmailAddresses = ListVerifiedEmailAddressesResponse

    request  = post "ListVerifiedEmailAddresses"
    response = xmlResponse

instance FromXML ListVerifiedEmailAddressesResponse where
    parseXML = withElement "ListVerifiedEmailAddressesResult" $ \x -> ListVerifiedEmailAddressesResponse
        <$> x .@  "VerifiedEmailAddresses"
