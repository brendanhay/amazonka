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

-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the descriptions of existing applications.
module Network.AWS.ElasticBeanstalk.DescribeApplications
    (
    -- * Request
      DescribeApplicationsMessage
    -- ** Request constructor
    , describeApplicationsMessage
    -- ** Request lenses
    , damApplicationNames

    -- * Response
    , ApplicationDescriptionsMessage
    -- ** Response constructor
    , applicationDescriptionsMessage
    -- ** Response lenses
    , admApplications
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

newtype DescribeApplicationsMessage = DescribeApplicationsMessage
    { _damApplicationNames :: [Text]
    } deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , Foldable
        , Traversable
        , Monoid
        , Semigroup
        )

-- | 'DescribeApplicationsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'damApplicationNames' @::@ ['Text']
--
describeApplicationsMessage :: DescribeApplicationsMessage
describeApplicationsMessage = DescribeApplicationsMessage
    { _damApplicationNames = mempty
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include those with the specified names.
damApplicationNames :: Lens' DescribeApplicationsMessage [Text]
damApplicationNames =
    lens _damApplicationNames (\s a -> s { _damApplicationNames = a })
instance ToQuery DescribeApplicationsMessage

instance ToPath DescribeApplicationsMessage where
    toPath = const "/"

newtype ApplicationDescriptionsMessage = ApplicationDescriptionsMessage
    { _admApplications :: [ApplicationDescription]
    } (Eq, Show, Generic, Foldable, Traversable, Monoid, Semigroup)

-- | 'ApplicationDescriptionsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'admApplications' @::@ ['ApplicationDescription']
--
applicationDescriptionsMessage :: ApplicationDescriptionsMessage
applicationDescriptionsMessage = ApplicationDescriptionsMessage
    { _admApplications = mempty
    }

-- | This parameter contains a list of ApplicationDescription.
admApplications :: Lens' ApplicationDescriptionsMessage [ApplicationDescription]
admApplications = lens _admApplications (\s a -> s { _admApplications = a })

instance FromXML ApplicationDescriptionsMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationDescriptionsMessage"

instance AWSRequest DescribeApplicationsMessage where
    type Sv DescribeApplicationsMessage = ElasticBeanstalk
    type Rs DescribeApplicationsMessage = ApplicationDescriptionsMessage

    request  = post "DescribeApplications"
    response = xmlResponse $ \h x -> ApplicationDescriptionsMessage
        <$> x %| "Applications"
