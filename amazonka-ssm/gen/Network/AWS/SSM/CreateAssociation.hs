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

-- Module      : Network.AWS.SSM.CreateAssociation
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

-- | Associates the specified configuration document with the specified instance.
--
-- When you associate a configuration document with an instance, the
-- configuration agent on the instance processes the configuration document and
-- configures the instance as specified.
--
-- If you associate a configuration document with an instance that already has
-- an associated configuration document, we replace the current configuration
-- document with the new configuration document.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_CreateAssociation.html>
module Network.AWS.SSM.CreateAssociation
    (
    -- * Request
      CreateAssociation
    -- ** Request constructor
    , createAssociation
    -- ** Request lenses
    , caInstanceId
    , caName

    -- * Response
    , CreateAssociationResponse
    -- ** Response constructor
    , createAssociationResponse
    -- ** Response lenses
    , carAssociationDescription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

data CreateAssociation = CreateAssociation
    { _caInstanceId :: Text
    , _caName       :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caInstanceId' @::@ 'Text'
--
-- * 'caName' @::@ 'Text'
--
createAssociation :: Text -- ^ 'caName'
                  -> Text -- ^ 'caInstanceId'
                  -> CreateAssociation
createAssociation p1 p2 = CreateAssociation
    { _caName       = p1
    , _caInstanceId = p2
    }

-- | The ID of the instance.
caInstanceId :: Lens' CreateAssociation Text
caInstanceId = lens _caInstanceId (\s a -> s { _caInstanceId = a })

-- | The name of the configuration document.
caName :: Lens' CreateAssociation Text
caName = lens _caName (\s a -> s { _caName = a })

newtype CreateAssociationResponse = CreateAssociationResponse
    { _carAssociationDescription :: Maybe AssociationDescription
    } deriving (Eq, Read, Show)

-- | 'CreateAssociationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carAssociationDescription' @::@ 'Maybe' 'AssociationDescription'
--
createAssociationResponse :: CreateAssociationResponse
createAssociationResponse = CreateAssociationResponse
    { _carAssociationDescription = Nothing
    }

-- | Information about the association.
carAssociationDescription :: Lens' CreateAssociationResponse (Maybe AssociationDescription)
carAssociationDescription =
    lens _carAssociationDescription
        (\s a -> s { _carAssociationDescription = a })

instance ToPath CreateAssociation where
    toPath = const "/"

instance ToQuery CreateAssociation where
    toQuery = const mempty

instance ToHeaders CreateAssociation

instance ToJSON CreateAssociation where
    toJSON CreateAssociation{..} = object
        [ "Name"       .= _caName
        , "InstanceId" .= _caInstanceId
        ]

instance AWSRequest CreateAssociation where
    type Sv CreateAssociation = SSM
    type Rs CreateAssociation = CreateAssociationResponse

    request  = post "CreateAssociation"
    response = jsonResponse

instance FromJSON CreateAssociationResponse where
    parseJSON = withObject "CreateAssociationResponse" $ \o -> CreateAssociationResponse
        <$> o .:? "AssociationDescription"
