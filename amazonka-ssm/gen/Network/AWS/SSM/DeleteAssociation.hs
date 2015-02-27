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

-- Module      : Network.AWS.SSM.DeleteAssociation
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

-- | Disassociates the specified configuration document from the specified
-- instance.
--
-- When you disassociate a configuration document from an instance, it does not
-- change the configuration of the instance. To change the configuration state
-- of an instance after you disassociate a configuration document, you must
-- create a new configuration document with the desired configuration and
-- associate it with the instance.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DeleteAssociation.html>
module Network.AWS.SSM.DeleteAssociation
    (
    -- * Request
      DeleteAssociation
    -- ** Request constructor
    , deleteAssociation
    -- ** Request lenses
    , da1InstanceId
    , da1Name

    -- * Response
    , DeleteAssociationResponse
    -- ** Response constructor
    , deleteAssociationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SSM.Types
import qualified GHC.Exts

data DeleteAssociation = DeleteAssociation
    { _da1InstanceId :: Text
    , _da1Name       :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'da1InstanceId' @::@ 'Text'
--
-- * 'da1Name' @::@ 'Text'
--
deleteAssociation :: Text -- ^ 'da1Name'
                  -> Text -- ^ 'da1InstanceId'
                  -> DeleteAssociation
deleteAssociation p1 p2 = DeleteAssociation
    { _da1Name       = p1
    , _da1InstanceId = p2
    }

-- | The ID of the instance.
da1InstanceId :: Lens' DeleteAssociation Text
da1InstanceId = lens _da1InstanceId (\s a -> s { _da1InstanceId = a })

-- | The name of the configuration document.
da1Name :: Lens' DeleteAssociation Text
da1Name = lens _da1Name (\s a -> s { _da1Name = a })

data DeleteAssociationResponse = DeleteAssociationResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteAssociationResponse' constructor.
deleteAssociationResponse :: DeleteAssociationResponse
deleteAssociationResponse = DeleteAssociationResponse

instance ToPath DeleteAssociation where
    toPath = const "/"

instance ToQuery DeleteAssociation where
    toQuery = const mempty

instance ToHeaders DeleteAssociation

instance ToJSON DeleteAssociation where
    toJSON DeleteAssociation{..} = object
        [ "Name"       .= _da1Name
        , "InstanceId" .= _da1InstanceId
        ]

instance AWSRequest DeleteAssociation where
    type Sv DeleteAssociation = SSM
    type Rs DeleteAssociation = DeleteAssociationResponse

    request  = post "DeleteAssociation"
    response = nullResponse DeleteAssociationResponse
