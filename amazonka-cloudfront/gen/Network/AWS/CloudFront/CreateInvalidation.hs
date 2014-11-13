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

-- Module      : Network.AWS.CloudFront.CreateInvalidation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new invalidation.
module Network.AWS.CloudFront.CreateInvalidation
    (
    -- * Request
      CreateInvalidation2014_05_31
    -- ** Request constructor
    , createInvalidation2014_05_31
    -- ** Request lenses
    , ciDistributionId
    , ciInvalidationBatch

    -- * Response
    , CreateInvalidation2014_05_31Response
    -- ** Response constructor
    , createInvalidation2014_05_31Response
    -- ** Response lenses
    , cirInvalidation
    , cirLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data CreateInvalidation2014_05_31 = CreateInvalidation2014_05_31
    { _ciDistributionId    :: Text
    , _ciInvalidationBatch :: InvalidationBatch
    } deriving (Eq, Show, Generic)

-- | 'CreateInvalidation2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciDistributionId' @::@ 'Text'
--
-- * 'ciInvalidationBatch' @::@ 'InvalidationBatch'
--
createInvalidation2014_05_31 :: Text -- ^ 'ciDistributionId'
                             -> InvalidationBatch -- ^ 'ciInvalidationBatch'
                             -> CreateInvalidation2014_05_31
createInvalidation2014_05_31 p1 p2 = CreateInvalidation2014_05_31
    { _ciDistributionId    = p1
    , _ciInvalidationBatch = p2
    }

-- | The distribution's id.
ciDistributionId :: Lens' CreateInvalidation2014_05_31 Text
ciDistributionId = lens _ciDistributionId (\s a -> s { _ciDistributionId = a })

-- | The batch information for the invalidation.
ciInvalidationBatch :: Lens' CreateInvalidation2014_05_31 InvalidationBatch
ciInvalidationBatch =
    lens _ciInvalidationBatch (\s a -> s { _ciInvalidationBatch = a })

instance ToPath CreateInvalidation2014_05_31 where
    toPath CreateInvalidation2014_05_31{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _ciDistributionId
        , "/invalidation"
        ]

instance ToQuery CreateInvalidation2014_05_31 where
    toQuery = const mempty

instance ToHeaders CreateInvalidation2014_05_31

instance ToBody CreateInvalidation2014_05_31 where
    toBody = toBody . encodeXML . _ciInvalidationBatch

data CreateInvalidation2014_05_31Response = CreateInvalidation2014_05_31Response
    { _cirInvalidation :: Maybe Invalidation
    , _cirLocation     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CreateInvalidation2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirInvalidation' @::@ 'Maybe' 'Invalidation'
--
-- * 'cirLocation' @::@ 'Maybe' 'Text'
--
createInvalidation2014_05_31Response :: CreateInvalidation2014_05_31Response
createInvalidation2014_05_31Response = CreateInvalidation2014_05_31Response
    { _cirLocation     = Nothing
    , _cirInvalidation = Nothing
    }

-- | The invalidation's information.
cirInvalidation :: Lens' CreateInvalidation2014_05_31Response (Maybe Invalidation)
cirInvalidation = lens _cirInvalidation (\s a -> s { _cirInvalidation = a })

-- | The fully qualified URI of the distribution and invalidation batch
-- request, including the Invalidation ID.
cirLocation :: Lens' CreateInvalidation2014_05_31Response (Maybe Text)
cirLocation = lens _cirLocation (\s a -> s { _cirLocation = a })

instance AWSRequest CreateInvalidation2014_05_31 where
    type Sv CreateInvalidation2014_05_31 = CloudFront
    type Rs CreateInvalidation2014_05_31 = CreateInvalidation2014_05_31Response

    request  = post
    response = xmlResponse $ \h x -> CreateInvalidation2014_05_31Response
        <$> x %| "Invalidation"
        <*> h ~:? "Location"
