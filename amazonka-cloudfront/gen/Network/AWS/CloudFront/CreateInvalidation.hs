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
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateInvalidation.html>
module Network.AWS.CloudFront.CreateInvalidation
    (
    -- * Request
      CreateInvalidation
    -- ** Request constructor
    , createInvalidation
    -- ** Request lenses
    , ciDistributionId
    , ciInvalidationBatch

    -- * Response
    , CreateInvalidationResponse
    -- ** Response constructor
    , createInvalidationResponse
    -- ** Response lenses
    , cirInvalidation
    , cirLocation
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data CreateInvalidation = CreateInvalidation
    { _ciDistributionId    :: Text
    , _ciInvalidationBatch :: InvalidationBatch
    } deriving (Eq, Show)

-- | 'CreateInvalidation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciDistributionId' @::@ 'Text'
--
-- * 'ciInvalidationBatch' @::@ 'InvalidationBatch'
--
createInvalidation :: Text -- ^ 'ciDistributionId'
                   -> InvalidationBatch -- ^ 'ciInvalidationBatch'
                   -> CreateInvalidation
createInvalidation p1 p2 = CreateInvalidation
    { _ciDistributionId    = p1
    , _ciInvalidationBatch = p2
    }

-- | The distribution's id.
ciDistributionId :: Lens' CreateInvalidation Text
ciDistributionId = lens _ciDistributionId (\s a -> s { _ciDistributionId = a })

-- | The batch information for the invalidation.
ciInvalidationBatch :: Lens' CreateInvalidation InvalidationBatch
ciInvalidationBatch =
    lens _ciInvalidationBatch (\s a -> s { _ciInvalidationBatch = a })

data CreateInvalidationResponse = CreateInvalidationResponse
    { _cirInvalidation :: Maybe Invalidation
    , _cirLocation     :: Maybe Text
    } deriving (Eq, Show)

-- | 'CreateInvalidationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cirInvalidation' @::@ 'Maybe' 'Invalidation'
--
-- * 'cirLocation' @::@ 'Maybe' 'Text'
--
createInvalidationResponse :: CreateInvalidationResponse
createInvalidationResponse = CreateInvalidationResponse
    { _cirLocation     = Nothing
    , _cirInvalidation = Nothing
    }

-- | The invalidation's information.
cirInvalidation :: Lens' CreateInvalidationResponse (Maybe Invalidation)
cirInvalidation = lens _cirInvalidation (\s a -> s { _cirInvalidation = a })

-- | The fully qualified URI of the distribution and invalidation batch request,
-- including the Invalidation ID.
cirLocation :: Lens' CreateInvalidationResponse (Maybe Text)
cirLocation = lens _cirLocation (\s a -> s { _cirLocation = a })

instance ToPath CreateInvalidation where
    toPath CreateInvalidation{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toText _ciDistributionId
        , "/invalidation"
        ]

instance ToQuery CreateInvalidation where
    toQuery = const mempty

instance ToHeaders CreateInvalidation

instance ToXMLRoot CreateInvalidation where
    toXMLRoot CreateInvalidation{..} = namespaced ns "CreateInvalidation"
        [ "InvalidationBatch" =@ _ciInvalidationBatch
        ]

instance ToXML CreateInvalidation

instance AWSRequest CreateInvalidation where
    type Sv CreateInvalidation = CloudFront
    type Rs CreateInvalidation = CreateInvalidationResponse

    request  = post
    response = xmlHeaderResponse $ \h x -> CreateInvalidationResponse
        <$> x .@? "Invalidation"
        <*> h ~:? "Location"
