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

-- Module      : Network.AWS.SSM.UpdateAssociationStatus
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

-- | Updates the status of the configuration document associated with the
-- specified instance.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_UpdateAssociationStatus.html>
module Network.AWS.SSM.UpdateAssociationStatus
    (
    -- * Request
      UpdateAssociationStatus
    -- ** Request constructor
    , updateAssociationStatus
    -- ** Request lenses
    , uasAssociationStatus
    , uasInstanceId
    , uasName

    -- * Response
    , UpdateAssociationStatusResponse
    -- ** Response constructor
    , updateAssociationStatusResponse
    -- ** Response lenses
    , uasrAssociationDescription
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

data UpdateAssociationStatus = UpdateAssociationStatus
    { _uasAssociationStatus :: AssociationStatus
    , _uasInstanceId        :: Text
    , _uasName              :: Text
    } deriving (Eq, Read, Show)

-- | 'UpdateAssociationStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uasAssociationStatus' @::@ 'AssociationStatus'
--
-- * 'uasInstanceId' @::@ 'Text'
--
-- * 'uasName' @::@ 'Text'
--
updateAssociationStatus :: Text -- ^ 'uasName'
                        -> Text -- ^ 'uasInstanceId'
                        -> AssociationStatus -- ^ 'uasAssociationStatus'
                        -> UpdateAssociationStatus
updateAssociationStatus p1 p2 p3 = UpdateAssociationStatus
    { _uasName              = p1
    , _uasInstanceId        = p2
    , _uasAssociationStatus = p3
    }

-- | The association status.
uasAssociationStatus :: Lens' UpdateAssociationStatus AssociationStatus
uasAssociationStatus =
    lens _uasAssociationStatus (\s a -> s { _uasAssociationStatus = a })

-- | The ID of the instance.
uasInstanceId :: Lens' UpdateAssociationStatus Text
uasInstanceId = lens _uasInstanceId (\s a -> s { _uasInstanceId = a })

-- | The name of the configuration document.
uasName :: Lens' UpdateAssociationStatus Text
uasName = lens _uasName (\s a -> s { _uasName = a })

newtype UpdateAssociationStatusResponse = UpdateAssociationStatusResponse
    { _uasrAssociationDescription :: Maybe AssociationDescription
    } deriving (Eq, Read, Show)

-- | 'UpdateAssociationStatusResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uasrAssociationDescription' @::@ 'Maybe' 'AssociationDescription'
--
updateAssociationStatusResponse :: UpdateAssociationStatusResponse
updateAssociationStatusResponse = UpdateAssociationStatusResponse
    { _uasrAssociationDescription = Nothing
    }

-- | Information about the association.
uasrAssociationDescription :: Lens' UpdateAssociationStatusResponse (Maybe AssociationDescription)
uasrAssociationDescription =
    lens _uasrAssociationDescription
        (\s a -> s { _uasrAssociationDescription = a })

instance ToPath UpdateAssociationStatus where
    toPath = const "/"

instance ToQuery UpdateAssociationStatus where
    toQuery = const mempty

instance ToHeaders UpdateAssociationStatus

instance ToJSON UpdateAssociationStatus where
    toJSON UpdateAssociationStatus{..} = object
        [ "Name"              .= _uasName
        , "InstanceId"        .= _uasInstanceId
        , "AssociationStatus" .= _uasAssociationStatus
        ]

instance AWSRequest UpdateAssociationStatus where
    type Sv UpdateAssociationStatus = SSM
    type Rs UpdateAssociationStatus = UpdateAssociationStatusResponse

    request  = post "UpdateAssociationStatus"
    response = jsonResponse

instance FromJSON UpdateAssociationStatusResponse where
    parseJSON = withObject "UpdateAssociationStatusResponse" $ \o -> UpdateAssociationStatusResponse
        <$> o .:? "AssociationDescription"
