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

-- Module      : Network.AWS.CloudHSM.ListHsms
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

-- | Retrieves the identifiers of all of the HSMs provisioned for the current
-- customer.
--
-- This operation supports pagination with the use of the /NextToken/ member. If
-- more results are available, the /NextToken/ member of the response contains a
-- token that you pass in the next call to 'ListHsms' to retrieve the next set of
-- items.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ListHsms.html>
module Network.AWS.CloudHSM.ListHsms
    (
    -- * Request
      ListHsms
    -- ** Request constructor
    , listHsms
    -- ** Request lenses
    , lh1NextToken

    -- * Response
    , ListHsmsResponse
    -- ** Response constructor
    , listHsmsResponse
    -- ** Response lenses
    , lhrHsmList
    , lhrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

newtype ListHsms = ListHsms
    { _lh1NextToken :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'ListHsms' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lh1NextToken' @::@ 'Maybe' 'Text'
--
listHsms :: ListHsms
listHsms = ListHsms
    { _lh1NextToken = Nothing
    }

-- | The /NextToken/ value from a previous call to 'ListHsms'. Pass null if this is
-- the first call.
lh1NextToken :: Lens' ListHsms (Maybe Text)
lh1NextToken = lens _lh1NextToken (\s a -> s { _lh1NextToken = a })

data ListHsmsResponse = ListHsmsResponse
    { _lhrHsmList   :: List "HsmList" Text
    , _lhrNextToken :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListHsmsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhrHsmList' @::@ ['Text']
--
-- * 'lhrNextToken' @::@ 'Maybe' 'Text'
--
listHsmsResponse :: ListHsmsResponse
listHsmsResponse = ListHsmsResponse
    { _lhrHsmList   = mempty
    , _lhrNextToken = Nothing
    }

-- | The list of ARNs that identify the HSMs.
lhrHsmList :: Lens' ListHsmsResponse [Text]
lhrHsmList = lens _lhrHsmList (\s a -> s { _lhrHsmList = a }) . _List

-- | If not null, more results are available. Pass this value to 'ListHsms' to
-- retrieve the next set of items.
lhrNextToken :: Lens' ListHsmsResponse (Maybe Text)
lhrNextToken = lens _lhrNextToken (\s a -> s { _lhrNextToken = a })

instance ToPath ListHsms where
    toPath = const "/"

instance ToQuery ListHsms where
    toQuery = const mempty

instance ToHeaders ListHsms

instance ToJSON ListHsms where
    toJSON ListHsms{..} = object
        [ "NextToken" .= _lh1NextToken
        ]

instance AWSRequest ListHsms where
    type Sv ListHsms = CloudHSM
    type Rs ListHsms = ListHsmsResponse

    request  = post "ListHsms"
    response = jsonResponse

instance FromJSON ListHsmsResponse where
    parseJSON = withObject "ListHsmsResponse" $ \o -> ListHsmsResponse
        <$> o .:? "HsmList" .!= mempty
        <*> o .:? "NextToken"
