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

-- Module      : Network.AWS.CloudHSM.ListHapgs
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

-- | Lists the high-availability partition groups for the account.
--
-- This operation supports pagination with the use of the /NextToken/ member. If
-- more results are available, the /NextToken/ member of the response contains a
-- token that you pass in the next call to 'ListHapgs' to retrieve the next set of
-- items.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ListHapgs.html>
module Network.AWS.CloudHSM.ListHapgs
    (
    -- * Request
      ListHapgs
    -- ** Request constructor
    , listHapgs
    -- ** Request lenses
    , lhNextToken

    -- * Response
    , ListHapgsResponse
    -- ** Response constructor
    , listHapgsResponse
    -- ** Response lenses
    , lhr1HapgList
    , lhr1NextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

newtype ListHapgs = ListHapgs
    { _lhNextToken :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'ListHapgs' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhNextToken' @::@ 'Maybe' 'Text'
--
listHapgs :: ListHapgs
listHapgs = ListHapgs
    { _lhNextToken = Nothing
    }

-- | The /NextToken/ value from a previous call to 'ListHapgs'. Pass null if this is
-- the first call.
lhNextToken :: Lens' ListHapgs (Maybe Text)
lhNextToken = lens _lhNextToken (\s a -> s { _lhNextToken = a })

data ListHapgsResponse = ListHapgsResponse
    { _lhr1HapgList  :: List "HapgList" Text
    , _lhr1NextToken :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListHapgsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhr1HapgList' @::@ ['Text']
--
-- * 'lhr1NextToken' @::@ 'Maybe' 'Text'
--
listHapgsResponse :: ListHapgsResponse
listHapgsResponse = ListHapgsResponse
    { _lhr1HapgList  = mempty
    , _lhr1NextToken = Nothing
    }

-- | The list of high-availability partition groups.
lhr1HapgList :: Lens' ListHapgsResponse [Text]
lhr1HapgList = lens _lhr1HapgList (\s a -> s { _lhr1HapgList = a }) . _List

-- | If not null, more results are available. Pass this value to 'ListHapgs' to
-- retrieve the next set of items.
lhr1NextToken :: Lens' ListHapgsResponse (Maybe Text)
lhr1NextToken = lens _lhr1NextToken (\s a -> s { _lhr1NextToken = a })

instance ToPath ListHapgs where
    toPath = const "/"

instance ToQuery ListHapgs where
    toQuery = const mempty

instance ToHeaders ListHapgs

instance ToJSON ListHapgs where
    toJSON ListHapgs{..} = object
        [ "NextToken" .= _lhNextToken
        ]

instance AWSRequest ListHapgs where
    type Sv ListHapgs = CloudHSM
    type Rs ListHapgs = ListHapgsResponse

    request  = post "ListHapgs"
    response = jsonResponse

instance FromJSON ListHapgsResponse where
    parseJSON = withObject "ListHapgsResponse" $ \o -> ListHapgsResponse
        <$> o .:? "HapgList" .!= mempty
        <*> o .:? "NextToken"
