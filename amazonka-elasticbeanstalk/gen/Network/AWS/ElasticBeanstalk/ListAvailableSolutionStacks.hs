{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the available solution stack names.
module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
    (
    -- * Request
      ListAvailableSolutionStacks
    -- ** Request constructor
    , listAvailableSolutionStacks

    -- * Response
    , ListAvailableSolutionStacksResponse
    -- ** Response constructor
    , listAvailableSolutionStacksResponse
    -- ** Response lenses
    , lassrSolutionStackDetails
    , lassrSolutionStacks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import qualified GHC.Exts

data ListAvailableSolutionStacks = ListAvailableSolutionStacks
    deriving (Eq, Ord, Show, Generic)

-- | 'ListAvailableSolutionStacks' constructor.
listAvailableSolutionStacks :: ListAvailableSolutionStacks
listAvailableSolutionStacks = ListAvailableSolutionStacks

instance ToQuery ListAvailableSolutionStacks

instance ToPath ListAvailableSolutionStacks where
    toPath = const "/"

data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse
    { _lassrSolutionStackDetails :: [SolutionStackDescription]
    , _lassrSolutionStacks       :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'ListAvailableSolutionStacksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lassrSolutionStackDetails' @::@ ['SolutionStackDescription']
--
-- * 'lassrSolutionStacks' @::@ ['Text']
--
listAvailableSolutionStacksResponse :: ListAvailableSolutionStacksResponse
listAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse
    { _lassrSolutionStacks       = mempty
    , _lassrSolutionStackDetails = mempty
    }

-- | A list of available solution stacks and their SolutionStackDescription.
lassrSolutionStackDetails :: Lens' ListAvailableSolutionStacksResponse [SolutionStackDescription]
lassrSolutionStackDetails =
    lens _lassrSolutionStackDetails
        (\s a -> s { _lassrSolutionStackDetails = a })

-- | A list of available solution stacks.
lassrSolutionStacks :: Lens' ListAvailableSolutionStacksResponse [Text]
lassrSolutionStacks =
    lens _lassrSolutionStacks (\s a -> s { _lassrSolutionStacks = a })

instance AWSRequest ListAvailableSolutionStacks where
    type Sv ListAvailableSolutionStacks = ElasticBeanstalk
    type Rs ListAvailableSolutionStacks = ListAvailableSolutionStacksResponse

    request  = post "ListAvailableSolutionStacks"
    response = xmlResponse $ \h x -> ListAvailableSolutionStacksResponse
        <$> x %| "SolutionStackDetails"
        <*> x %| "SolutionStacks"
