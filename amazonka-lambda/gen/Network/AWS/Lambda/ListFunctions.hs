{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.ListFunctions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of your Lambda functions. For each function, the response
-- includes the function configuration information. You must use GetFunction
-- to retrieve the code for your function. This operation requires permission
-- for the lambda:ListFunctions action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_ListFunctions.html>
module Network.AWS.Lambda.ListFunctions
    (
    -- * Request
      ListFunctions
    -- ** Request constructor
    , listFunctions
    -- ** Request lenses
    , lfMarker
    , lfMaxItems

    -- * Response
    , ListFunctionsResponse
    -- ** Response constructor
    , listFunctionsResponse
    -- ** Response lenses
    , lfrFunctions
    , lfrNextMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data ListFunctions = ListFunctions
    { _lfMarker   :: Maybe Text
    , _lfMaxItems :: Maybe Nat
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListFunctions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lfMarker' @::@ 'Maybe' 'Text'
--
-- * 'lfMaxItems' @::@ 'Maybe' 'Natural'
--
listFunctions :: ListFunctions
listFunctions = ListFunctions
    { _lfMarker   = Nothing
    , _lfMaxItems = Nothing
    }

-- | Optional string. An opaque pagination token returned from a previous
-- ListFunctions operation. If present, indicates where to continue the
-- listing.
lfMarker :: Lens' ListFunctions (Maybe Text)
lfMarker = lens _lfMarker (\s a -> s { _lfMarker = a })

-- | Optional integer. Specifies the maximum number of AWS Lambda functions to
-- return in response. This parameter value must be greater than 0.
lfMaxItems :: Lens' ListFunctions (Maybe Natural)
lfMaxItems = lens _lfMaxItems (\s a -> s { _lfMaxItems = a })
    . mapping _Nat

data ListFunctionsResponse = ListFunctionsResponse
    { _lfrFunctions  :: [FunctionConfiguration]
    , _lfrNextMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListFunctionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lfrFunctions' @::@ ['FunctionConfiguration']
--
-- * 'lfrNextMarker' @::@ 'Maybe' 'Text'
--
listFunctionsResponse :: ListFunctionsResponse
listFunctionsResponse = ListFunctionsResponse
    { _lfrNextMarker = Nothing
    , _lfrFunctions  = mempty
    }

-- | A list of Lambda functions.
lfrFunctions :: Lens' ListFunctionsResponse [FunctionConfiguration]
lfrFunctions = lens _lfrFunctions (\s a -> s { _lfrFunctions = a })

-- | A string, present if there are more functions.
lfrNextMarker :: Lens' ListFunctionsResponse (Maybe Text)
lfrNextMarker = lens _lfrNextMarker (\s a -> s { _lfrNextMarker = a })

instance ToPath ListFunctions where
    toPath = const "/2014-11-13/functions/"

instance ToQuery ListFunctions

instance ToHeaders ListFunctions

instance ToJSON ListFunctions where
    toJSON = const Null

instance AWSRequest ListFunctions where
    type Sv ListFunctions = Lambda
    type Rs ListFunctions = ListFunctionsResponse

    request  = get
    response = jsonResponse

instance FromJSON ListFunctionsResponse where
    parseJSON = withObject "ListFunctionsResponse" $ \o -> ListFunctionsResponse
        <$> o .: "Functions"
        <*> o .:? "NextMarker"
