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

-- Module      : Network.AWS.CloudSearch.DefineSuggester
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures a suggester for a domain. A suggester enables you to display
-- possible matches before users finish typing their queries. When you
-- configure a suggester, you must specify the name of the text field you want
-- to search for possible matches and a unique name for the suggester. For
-- more information, see Getting Search Suggestions in the Amazon CloudSearch
-- Developer Guide.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineSuggester.html>
module Network.AWS.CloudSearch.DefineSuggester
    (
    -- * Request
      DefineSuggester
    -- ** Request constructor
    , defineSuggester
    -- ** Request lenses
    , ds2DomainName
    , ds2Suggester

    -- * Response
    , DefineSuggesterResponse
    -- ** Response constructor
    , defineSuggesterResponse
    -- ** Response lenses
    , dsrSuggester
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DefineSuggester = DefineSuggester
    { _ds2DomainName :: Text
    , _ds2Suggester  :: Suggester
    } deriving (Eq, Show)

-- | 'DefineSuggester' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds2DomainName' @::@ 'Text'
--
-- * 'ds2Suggester' @::@ 'Suggester'
--
defineSuggester :: Text -- ^ 'ds2DomainName'
                -> Suggester -- ^ 'ds2Suggester'
                -> DefineSuggester
defineSuggester p1 p2 = DefineSuggester
    { _ds2DomainName = p1
    , _ds2Suggester  = p2
    }

ds2DomainName :: Lens' DefineSuggester Text
ds2DomainName = lens _ds2DomainName (\s a -> s { _ds2DomainName = a })

ds2Suggester :: Lens' DefineSuggester Suggester
ds2Suggester = lens _ds2Suggester (\s a -> s { _ds2Suggester = a })

newtype DefineSuggesterResponse = DefineSuggesterResponse
    { _dsrSuggester :: SuggesterStatus
    } deriving (Eq, Show)

-- | 'DefineSuggesterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrSuggester' @::@ 'SuggesterStatus'
--
defineSuggesterResponse :: SuggesterStatus -- ^ 'dsrSuggester'
                        -> DefineSuggesterResponse
defineSuggesterResponse p1 = DefineSuggesterResponse
    { _dsrSuggester = p1
    }

dsrSuggester :: Lens' DefineSuggesterResponse SuggesterStatus
dsrSuggester = lens _dsrSuggester (\s a -> s { _dsrSuggester = a })

instance ToPath DefineSuggester where
    toPath = const "/"

instance ToQuery DefineSuggester where
    toQuery DefineSuggester{..} = mconcat
        [ "DomainName" =? _ds2DomainName
        , "Suggester"  =? _ds2Suggester
        ]

instance ToHeaders DefineSuggester

instance AWSRequest DefineSuggester where
    type Sv DefineSuggester = CloudSearch
    type Rs DefineSuggester = DefineSuggesterResponse

    request  = post "DefineSuggester"
    response = xmlResponse

instance FromXML DefineSuggesterResponse where
    parseXML = withElement "DefineSuggesterResult" $ \x -> DefineSuggesterResponse
        <$> x .@  "Suggester"


Some kind of operator / class to check the types whether to continue?
