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

-- Module      : Network.AWS.ECS.DescribeTaskDefinition
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes a task definition. You can specify a 'family' and 'revision' to find
-- information on a specific task definition, or you can simply specify the
-- family to find the latest revision in that family.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DescribeTaskDefinition.html>
module Network.AWS.ECS.DescribeTaskDefinition
    (
    -- * Request
      DescribeTaskDefinition
    -- ** Request constructor
    , describeTaskDefinition
    -- ** Request lenses
    , dtdTaskDefinition

    -- * Response
    , DescribeTaskDefinitionResponse
    -- ** Response constructor
    , describeTaskDefinitionResponse
    -- ** Response lenses
    , dtdr1TaskDefinition
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

newtype DescribeTaskDefinition = DescribeTaskDefinition
    { _dtdTaskDefinition :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DescribeTaskDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtdTaskDefinition' @::@ 'Text'
--
describeTaskDefinition :: Text -- ^ 'dtdTaskDefinition'
                       -> DescribeTaskDefinition
describeTaskDefinition p1 = DescribeTaskDefinition
    { _dtdTaskDefinition = p1
    }

-- | The 'family' for the latest revision, 'family' and 'revision' ('family:revision') for
-- a specific revision in the family, or full Amazon Resource Name (ARN) of the
-- task definition that you want to describe.
dtdTaskDefinition :: Lens' DescribeTaskDefinition Text
dtdTaskDefinition =
    lens _dtdTaskDefinition (\s a -> s { _dtdTaskDefinition = a })

newtype DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse
    { _dtdr1TaskDefinition :: Maybe TaskDefinition
    } deriving (Eq, Read, Show)

-- | 'DescribeTaskDefinitionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtdr1TaskDefinition' @::@ 'Maybe' 'TaskDefinition'
--
describeTaskDefinitionResponse :: DescribeTaskDefinitionResponse
describeTaskDefinitionResponse = DescribeTaskDefinitionResponse
    { _dtdr1TaskDefinition = Nothing
    }

-- | The full task definition description.
dtdr1TaskDefinition :: Lens' DescribeTaskDefinitionResponse (Maybe TaskDefinition)
dtdr1TaskDefinition =
    lens _dtdr1TaskDefinition (\s a -> s { _dtdr1TaskDefinition = a })

instance ToPath DescribeTaskDefinition where
    toPath = const "/"

instance ToQuery DescribeTaskDefinition where
    toQuery = const mempty

instance ToHeaders DescribeTaskDefinition

instance ToJSON DescribeTaskDefinition where
    toJSON DescribeTaskDefinition{..} = object
        [ "taskDefinition" .= _dtdTaskDefinition
        ]

instance AWSRequest DescribeTaskDefinition where
    type Sv DescribeTaskDefinition = ECS
    type Rs DescribeTaskDefinition = DescribeTaskDefinitionResponse

    request  = post "DescribeTaskDefinition"
    response = jsonResponse

instance FromJSON DescribeTaskDefinitionResponse where
    parseJSON = withObject "DescribeTaskDefinitionResponse" $ \o -> DescribeTaskDefinitionResponse
        <$> o .:? "taskDefinition"
