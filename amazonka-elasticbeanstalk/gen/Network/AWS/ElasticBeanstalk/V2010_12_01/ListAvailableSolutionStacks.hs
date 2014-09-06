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
    , mkListAvailableSolutionStacks
    -- * Response
    , ListAvailableSolutionStacksResponse
    -- ** Response lenses
    , lassrsSolutionStacks
    , lassrsSolutionStackDetails
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

data ListAvailableSolutionStacks = ListAvailableSolutionStacks
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListAvailableSolutionStacks' request.
mkListAvailableSolutionStacks :: ListAvailableSolutionStacks
mkListAvailableSolutionStacks = ListAvailableSolutionStacks
{-# INLINE mkListAvailableSolutionStacks #-}

instance ToQuery ListAvailableSolutionStacks where
    toQuery = genericQuery def

-- | A list of available AWS Elastic Beanstalk solution stacks.
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse
    { _lassrsSolutionStacks :: [Text]
    , _lassrsSolutionStackDetails :: [SolutionStackDescription]
    } deriving (Show, Generic)

-- | A list of available solution stacks.
lassrsSolutionStacks :: Lens' ListAvailableSolutionStacksResponse [Text]
lassrsSolutionStacks =
    lens _lassrsSolutionStacks (\s a -> s { _lassrsSolutionStacks = a })
{-# INLINE lassrsSolutionStacks #-}

-- | A list of available solution stacks and their SolutionStackDescription.
lassrsSolutionStackDetails :: Lens' ListAvailableSolutionStacksResponse [SolutionStackDescription]
lassrsSolutionStackDetails =
    lens _lassrsSolutionStackDetails
         (\s a -> s { _lassrsSolutionStackDetails = a })
{-# INLINE lassrsSolutionStackDetails #-}

instance FromXML ListAvailableSolutionStacksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListAvailableSolutionStacks where
    type Sv ListAvailableSolutionStacks = ElasticBeanstalk
    type Rs ListAvailableSolutionStacks = ListAvailableSolutionStacksResponse

    request = post "ListAvailableSolutionStacks"
    response _ = xmlResponse
