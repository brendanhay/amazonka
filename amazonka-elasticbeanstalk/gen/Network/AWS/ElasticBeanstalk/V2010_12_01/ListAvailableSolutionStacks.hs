{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.ListAvailableSolutionStacks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the available solution stack names.
-- https://elasticbeanstalk.us-east-1.amazon.com/?Operation=ListAvailableSolutionStacks
-- &AuthParams 64bit Amazon Linux running Tomcat 6 32bit Amazon Linux running
-- Tomcat 6 64bit Amazon Linux running Tomcat 7 32bit Amazon Linux running
-- Tomcat 7 f21e2a92-f1fc-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.ListAvailableSolutionStacks
    (
    -- * Request
      ListAvailableSolutionStacks
    -- ** Request constructor
    , listAvailableSolutionStacks
    -- * Response
    , ListAvailableSolutionStacksResponse
    -- ** Response lenses
    , lassrmSolutionStackDetails
    , lassrmSolutionStacks
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListAvailableSolutionStacks' request.
listAvailableSolutionStacks :: ListAvailableSolutionStacks
listAvailableSolutionStacks = ListAvailableSolutionStacks

data ListAvailableSolutionStacks = ListAvailableSolutionStacks
    deriving (Eq, Show, Generic)

instance ToQuery ListAvailableSolutionStacks where
    toQuery = genericQuery def

data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse
    { _lassrmSolutionStackDetails :: [SolutionStackDescription]
      -- ^ A list of available solution stacks and their
      -- SolutionStackDescription.
    , _lassrmSolutionStacks :: [Text]
      -- ^ A list of available solution stacks.
    } deriving (Show, Generic)

-- | A list of available solution stacks and their SolutionStackDescription.
lassrmSolutionStackDetails
    :: Functor f
    => ([SolutionStackDescription]
    -> f ([SolutionStackDescription]))
    -> ListAvailableSolutionStacksResponse
    -> f ListAvailableSolutionStacksResponse
lassrmSolutionStackDetails f x =
    (\y -> x { _lassrmSolutionStackDetails = y })
       <$> f (_lassrmSolutionStackDetails x)
{-# INLINE lassrmSolutionStackDetails #-}

-- | A list of available solution stacks.
lassrmSolutionStacks
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ListAvailableSolutionStacksResponse
    -> f ListAvailableSolutionStacksResponse
lassrmSolutionStacks f x =
    (\y -> x { _lassrmSolutionStacks = y })
       <$> f (_lassrmSolutionStacks x)
{-# INLINE lassrmSolutionStacks #-}

instance FromXML ListAvailableSolutionStacksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListAvailableSolutionStacks where
    type Sv ListAvailableSolutionStacks = ElasticBeanstalk
    type Rs ListAvailableSolutionStacks = ListAvailableSolutionStacksResponse

    request = post "ListAvailableSolutionStacks"
    response _ = xmlResponse
