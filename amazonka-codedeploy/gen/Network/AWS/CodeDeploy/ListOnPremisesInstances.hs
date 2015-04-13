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

-- Module      : Network.AWS.CodeDeploy.ListOnPremisesInstances
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

-- | Gets a list of one or more on-premises instance names.
--
-- Unless otherwise specified, both registered and deregistered on-premises
-- instance names will be listed. To list only registered or deregistered
-- on-premises instance names, use the registration status parameter.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListOnPremisesInstances.html>
module Network.AWS.CodeDeploy.ListOnPremisesInstances
    (
    -- * Request
      ListOnPremisesInstances
    -- ** Request constructor
    , listOnPremisesInstances
    -- ** Request lenses
    , lopiNextToken
    , lopiRegistrationStatus
    , lopiTagFilters

    -- * Response
    , ListOnPremisesInstancesResponse
    -- ** Response constructor
    , listOnPremisesInstancesResponse
    -- ** Response lenses
    , lopirInstanceNames
    , lopirNextToken
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data ListOnPremisesInstances = ListOnPremisesInstances
    { _lopiNextToken          :: Maybe Text
    , _lopiRegistrationStatus :: Maybe RegistrationStatus
    , _lopiTagFilters         :: List "tagFilters" TagFilter
    } deriving (Eq, Read, Show)

-- | 'ListOnPremisesInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lopiNextToken' @::@ 'Maybe' 'Text'
--
-- * 'lopiRegistrationStatus' @::@ 'Maybe' 'RegistrationStatus'
--
-- * 'lopiTagFilters' @::@ ['TagFilter']
--
listOnPremisesInstances :: ListOnPremisesInstances
listOnPremisesInstances = ListOnPremisesInstances
    { _lopiRegistrationStatus = Nothing
    , _lopiTagFilters         = mempty
    , _lopiNextToken          = Nothing
    }

-- | An identifier that was returned from the previous list on-premises instances
-- call, which can be used to return the next set of on-premises instances in
-- the list.
lopiNextToken :: Lens' ListOnPremisesInstances (Maybe Text)
lopiNextToken = lens _lopiNextToken (\s a -> s { _lopiNextToken = a })

-- | The on-premises instances registration status:
--
-- Deregistered: Include in the resulting list deregistered on-premises
-- instances. Registered: Include in the resulting list registered on-premises
-- instances.
lopiRegistrationStatus :: Lens' ListOnPremisesInstances (Maybe RegistrationStatus)
lopiRegistrationStatus =
    lens _lopiRegistrationStatus (\s a -> s { _lopiRegistrationStatus = a })

-- | The on-premises instance tags that will be used to restrict the corresponding
-- on-premises instance names that are returned.
lopiTagFilters :: Lens' ListOnPremisesInstances [TagFilter]
lopiTagFilters = lens _lopiTagFilters (\s a -> s { _lopiTagFilters = a }) . _List

data ListOnPremisesInstancesResponse = ListOnPremisesInstancesResponse
    { _lopirInstanceNames :: List "instanceNames" Text
    , _lopirNextToken     :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListOnPremisesInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lopirInstanceNames' @::@ ['Text']
--
-- * 'lopirNextToken' @::@ 'Maybe' 'Text'
--
listOnPremisesInstancesResponse :: ListOnPremisesInstancesResponse
listOnPremisesInstancesResponse = ListOnPremisesInstancesResponse
    { _lopirInstanceNames = mempty
    , _lopirNextToken     = Nothing
    }

-- | The list of matching on-premises instance names.
lopirInstanceNames :: Lens' ListOnPremisesInstancesResponse [Text]
lopirInstanceNames =
    lens _lopirInstanceNames (\s a -> s { _lopirInstanceNames = a })
        . _List

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- on-premises instances call to return the next set of on-premises instances in
-- the list.
lopirNextToken :: Lens' ListOnPremisesInstancesResponse (Maybe Text)
lopirNextToken = lens _lopirNextToken (\s a -> s { _lopirNextToken = a })

instance ToPath ListOnPremisesInstances where
    toPath = const "/"

instance ToQuery ListOnPremisesInstances where
    toQuery = const mempty

instance ToHeaders ListOnPremisesInstances

instance ToJSON ListOnPremisesInstances where
    toJSON ListOnPremisesInstances{..} = object
        [ "registrationStatus" .= _lopiRegistrationStatus
        , "tagFilters"         .= _lopiTagFilters
        , "nextToken"          .= _lopiNextToken
        ]

instance AWSRequest ListOnPremisesInstances where
    type Sv ListOnPremisesInstances = CodeDeploy
    type Rs ListOnPremisesInstances = ListOnPremisesInstancesResponse

    request  = post "ListOnPremisesInstances"
    response = jsonResponse

instance FromJSON ListOnPremisesInstancesResponse where
    parseJSON = withObject "ListOnPremisesInstancesResponse" $ \o -> ListOnPremisesInstancesResponse
        <$> o .:? "instanceNames" .!= mempty
        <*> o .:? "nextToken"
