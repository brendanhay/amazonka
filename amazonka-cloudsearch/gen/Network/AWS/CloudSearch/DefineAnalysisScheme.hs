{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CloudSearch.DefineAnalysisScheme
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures an analysis scheme that can be applied to a text or text-array
-- field to define language-specific text processing options. For more
-- information, see Configuring Analysis Schemes in the Amazon CloudSearch
-- Developer Guide.
module Network.AWS.CloudSearch.DefineAnalysisScheme
    (
    -- * Request
      DefineAnalysisScheme
    -- ** Request constructor
    , defineAnalysisScheme
    -- ** Request lenses
    , das2AnalysisScheme
    , das2DomainName

    -- * Response
    , DefineAnalysisSchemeResponse
    -- ** Response constructor
    , defineAnalysisSchemeResponse
    -- ** Response lenses
    , dasr1AnalysisScheme
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DefineAnalysisScheme = DefineAnalysisScheme
    { _das2AnalysisScheme :: AnalysisScheme
    , _das2DomainName     :: Text
    } deriving (Eq, Show, Generic)

-- | 'DefineAnalysisScheme' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'das2AnalysisScheme' @::@ 'AnalysisScheme'
--
-- * 'das2DomainName' @::@ 'Text'
--
defineAnalysisScheme :: Text -- ^ 'das2DomainName'
                     -> AnalysisScheme -- ^ 'das2AnalysisScheme'
                     -> DefineAnalysisScheme
defineAnalysisScheme p1 p2 = DefineAnalysisScheme
    { _das2DomainName     = p1
    , _das2AnalysisScheme = p2
    }

das2AnalysisScheme :: Lens' DefineAnalysisScheme AnalysisScheme
das2AnalysisScheme =
    lens _das2AnalysisScheme (\s a -> s { _das2AnalysisScheme = a })

das2DomainName :: Lens' DefineAnalysisScheme Text
das2DomainName = lens _das2DomainName (\s a -> s { _das2DomainName = a })

instance ToQuery DefineAnalysisScheme

instance ToPath DefineAnalysisScheme where
    toPath = const "/"

newtype DefineAnalysisSchemeResponse = DefineAnalysisSchemeResponse
    { _dasr1AnalysisScheme :: AnalysisSchemeStatus
    } deriving (Eq, Show, Generic)

-- | 'DefineAnalysisSchemeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasr1AnalysisScheme' @::@ 'AnalysisSchemeStatus'
--
defineAnalysisSchemeResponse :: AnalysisSchemeStatus -- ^ 'dasr1AnalysisScheme'
                             -> DefineAnalysisSchemeResponse
defineAnalysisSchemeResponse p1 = DefineAnalysisSchemeResponse
    { _dasr1AnalysisScheme = p1
    }

dasr1AnalysisScheme :: Lens' DefineAnalysisSchemeResponse AnalysisSchemeStatus
dasr1AnalysisScheme =
    lens _dasr1AnalysisScheme (\s a -> s { _dasr1AnalysisScheme = a })

instance AWSRequest DefineAnalysisScheme where
    type Sv DefineAnalysisScheme = CloudSearch
    type Rs DefineAnalysisScheme = DefineAnalysisSchemeResponse

    request  = post "DefineAnalysisScheme"
    response = xmlResponse $ \h x -> DefineAnalysisSchemeResponse
        <$> x %| "AnalysisScheme"
