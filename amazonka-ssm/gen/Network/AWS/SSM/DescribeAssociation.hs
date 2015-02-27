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

-- Module      : Network.AWS.SSM.DescribeAssociation
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

-- | Describes the associations for the specified configuration document or
-- instance.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DescribeAssociation.html>
module Network.AWS.SSM.DescribeAssociation
    (
    -- * Request
      DescribeAssociation
    -- ** Request constructor
    , describeAssociation
    -- ** Request lenses
    , daInstanceId
    , daName

    -- * Response
    , DescribeAssociationResponse
    -- ** Response constructor
    , describeAssociationResponse
    -- ** Response lenses
    , darAssociationDescription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

data DescribeAssociation = DescribeAssociation
    { _daInstanceId :: Text
    , _daName       :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daInstanceId' @::@ 'Text'
--
-- * 'daName' @::@ 'Text'
--
describeAssociation :: Text -- ^ 'daName'
                    -> Text -- ^ 'daInstanceId'
                    -> DescribeAssociation
describeAssociation p1 p2 = DescribeAssociation
    { _daName       = p1
    , _daInstanceId = p2
    }

-- | The ID of the instance.
daInstanceId :: Lens' DescribeAssociation Text
daInstanceId = lens _daInstanceId (\s a -> s { _daInstanceId = a })

-- | The name of the configuration document.
daName :: Lens' DescribeAssociation Text
daName = lens _daName (\s a -> s { _daName = a })

newtype DescribeAssociationResponse = DescribeAssociationResponse
    { _darAssociationDescription :: Maybe AssociationDescription
    } deriving (Eq, Read, Show)

-- | 'DescribeAssociationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darAssociationDescription' @::@ 'Maybe' 'AssociationDescription'
--
describeAssociationResponse :: DescribeAssociationResponse
describeAssociationResponse = DescribeAssociationResponse
    { _darAssociationDescription = Nothing
    }

-- | Information about the association.
darAssociationDescription :: Lens' DescribeAssociationResponse (Maybe AssociationDescription)
darAssociationDescription =
    lens _darAssociationDescription
        (\s a -> s { _darAssociationDescription = a })

instance ToPath DescribeAssociation where
    toPath = const "/"

instance ToQuery DescribeAssociation where
    toQuery = const mempty

instance ToHeaders DescribeAssociation

instance ToJSON DescribeAssociation where
    toJSON DescribeAssociation{..} = object
        [ "Name"       .= _daName
        , "InstanceId" .= _daInstanceId
        ]

instance AWSRequest DescribeAssociation where
    type Sv DescribeAssociation = SSM
    type Rs DescribeAssociation = DescribeAssociationResponse

    request  = post "DescribeAssociation"
    response = jsonResponse

instance FromJSON DescribeAssociationResponse where
    parseJSON = withObject "DescribeAssociationResponse" $ \o -> DescribeAssociationResponse
        <$> o .:? "AssociationDescription"
