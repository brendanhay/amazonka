{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SSM.DeleteAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- When you disassociate a configuration document from an instance, it does
-- not change the configuration of the instance. To change the
-- configuration state of an instance after you disassociate a
-- configuration document, you must create a new configuration document
-- with the desired configuration and associate it with the instance.
--
-- <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DeleteAssociation.html>
module Network.AWS.SSM.DeleteAssociation
    (
    -- * Request
      DeleteAssociation
    -- ** Request constructor
    , deleteAssociation
    -- ** Request lenses
    , delName
    , delInstanceId

    -- * Response
    , DeleteAssociationResponse
    -- ** Response constructor
    , deleteAssociationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'deleteAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delName'
--
-- * 'delInstanceId'
data DeleteAssociation = DeleteAssociation'{_delName :: Text, _delInstanceId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteAssociation' smart constructor.
deleteAssociation :: Text -> Text -> DeleteAssociation
deleteAssociation pName pInstanceId = DeleteAssociation'{_delName = pName, _delInstanceId = pInstanceId};

-- | The name of the configuration document.
delName :: Lens' DeleteAssociation Text
delName = lens _delName (\ s a -> s{_delName = a});

-- | The ID of the instance.
delInstanceId :: Lens' DeleteAssociation Text
delInstanceId = lens _delInstanceId (\ s a -> s{_delInstanceId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteAssociation where
        type Sv DeleteAssociation = SSM
        type Rs DeleteAssociation = DeleteAssociationResponse
        request = postJSON
        response = receiveNull DeleteAssociationResponse'

instance ToHeaders DeleteAssociation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeleteAssociation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAssociation where
        toJSON DeleteAssociation'{..}
          = object
              ["Name" .= _delName, "InstanceId" .= _delInstanceId]

instance ToPath DeleteAssociation where
        toPath = const "/"

instance ToQuery DeleteAssociation where
        toQuery = const mempty

-- | /See:/ 'deleteAssociationResponse' smart constructor.
data DeleteAssociationResponse = DeleteAssociationResponse' deriving (Eq, Read, Show)

-- | 'DeleteAssociationResponse' smart constructor.
deleteAssociationResponse :: DeleteAssociationResponse
deleteAssociationResponse = DeleteAssociationResponse';
