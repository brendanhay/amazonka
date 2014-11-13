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

-- Module      : Network.AWS.CloudSearch.DeleteSuggester
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a suggester. For more information, see Getting Search Suggestions
-- in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.DeleteSuggester
    (
    -- * Request
      DeleteSuggester
    -- ** Request constructor
    , deleteSuggester
    -- ** Request lenses
    , ds3DomainName
    , ds3SuggesterName

    -- * Response
    , DeleteSuggesterResponse
    -- ** Response constructor
    , deleteSuggesterResponse
    -- ** Response lenses
    , dsr1Suggester
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DeleteSuggester = DeleteSuggester
    { _ds3DomainName    :: Text
    , _ds3SuggesterName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteSuggester' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds3DomainName' @::@ 'Text'
--
-- * 'ds3SuggesterName' @::@ 'Text'
--
deleteSuggester :: Text -- ^ 'ds3DomainName'
                -> Text -- ^ 'ds3SuggesterName'
                -> DeleteSuggester
deleteSuggester p1 p2 = DeleteSuggester
    { _ds3DomainName    = p1
    , _ds3SuggesterName = p2
    }

ds3DomainName :: Lens' DeleteSuggester Text
ds3DomainName = lens _ds3DomainName (\s a -> s { _ds3DomainName = a })

-- | Specifies the name of the suggester you want to delete.
ds3SuggesterName :: Lens' DeleteSuggester Text
ds3SuggesterName = lens _ds3SuggesterName (\s a -> s { _ds3SuggesterName = a })

instance ToQuery DeleteSuggester

instance ToPath DeleteSuggester where
    toPath = const "/"

newtype DeleteSuggesterResponse = DeleteSuggesterResponse
    { _dsr1Suggester :: SuggesterStatus
    } deriving (Eq, Show, Generic)

-- | 'DeleteSuggesterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsr1Suggester' @::@ 'SuggesterStatus'
--
deleteSuggesterResponse :: SuggesterStatus -- ^ 'dsr1Suggester'
                        -> DeleteSuggesterResponse
deleteSuggesterResponse p1 = DeleteSuggesterResponse
    { _dsr1Suggester = p1
    }

-- | The status of the suggester being deleted.
dsr1Suggester :: Lens' DeleteSuggesterResponse SuggesterStatus
dsr1Suggester = lens _dsr1Suggester (\s a -> s { _dsr1Suggester = a })

instance AWSRequest DeleteSuggester where
    type Sv DeleteSuggester = CloudSearch
    type Rs DeleteSuggester = DeleteSuggesterResponse

    request  = post "DeleteSuggester"
    response = xmlResponse $ \h x -> DeleteSuggesterResponse
        <$> x %| "Suggester"
