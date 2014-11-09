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
    , ListAvailableSolutionStacksResultMessage
    -- ** Response constructor
    , listAvailableSolutionStacksResultMessage
    -- ** Response lenses
    , lassrmSolutionStackDetails
    , lassrmSolutionStacks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

data ListAvailableSolutionStacks = ListAvailableSolutionStacks

-- | 'ListAvailableSolutionStacks' constructor.
listAvailableSolutionStacks :: ListAvailableSolutionStacks
listAvailableSolutionStacks = ListAvailableSolutionStacks

instance ToPath ListAvailableSolutionStacks where
    toPath = const "/"

instance ToQuery ListAvailableSolutionStacks

data ListAvailableSolutionStacksResultMessage = ListAvailableSolutionStacksResultMessage
    { _lassrmSolutionStackDetails :: [SolutionStackDescription]
    , _lassrmSolutionStacks       :: [Text]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'ListAvailableSolutionStacksResultMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lassrmSolutionStackDetails' @::@ ['SolutionStackDescription']
--
-- * 'lassrmSolutionStacks' @::@ ['Text']
--
listAvailableSolutionStacksResultMessage :: ListAvailableSolutionStacksResultMessage
listAvailableSolutionStacksResultMessage = ListAvailableSolutionStacksResultMessage
    { _lassrmSolutionStacks       = mempty
    , _lassrmSolutionStackDetails = mempty
    }

-- | A list of available solution stacks and their SolutionStackDescription.
lassrmSolutionStackDetails :: Lens' ListAvailableSolutionStacksResultMessage [SolutionStackDescription]
lassrmSolutionStackDetails =
    lens _lassrmSolutionStackDetails
        (\s a -> s { _lassrmSolutionStackDetails = a })

-- | A list of available solution stacks.
lassrmSolutionStacks :: Lens' ListAvailableSolutionStacksResultMessage [Text]
lassrmSolutionStacks =
    lens _lassrmSolutionStacks (\s a -> s { _lassrmSolutionStacks = a })

instance AWSRequest ListAvailableSolutionStacks where
    type Sv ListAvailableSolutionStacks = ElasticBeanstalk
    type Rs ListAvailableSolutionStacks = ListAvailableSolutionStacksResultMessage

    request  = post "ListAvailableSolutionStacks"
    response = const . xmlResponse $ \h x -> ListAvailableSolutionStacksResultMessage
record
