{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    -- ** Response lenses
    , delStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'deleteAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delName'
--
-- * 'delInstanceId'
data DeleteAssociation = DeleteAssociation'
    { _delName       :: !Text
    , _delInstanceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAssociation' smart constructor.
deleteAssociation :: Text -> Text -> DeleteAssociation
deleteAssociation pName pInstanceId =
    DeleteAssociation'
    { _delName = pName
    , _delInstanceId = pInstanceId
    }

-- | The name of the configuration document.
delName :: Lens' DeleteAssociation Text
delName = lens _delName (\ s a -> s{_delName = a});

-- | The ID of the instance.
delInstanceId :: Lens' DeleteAssociation Text
delInstanceId = lens _delInstanceId (\ s a -> s{_delInstanceId = a});

instance AWSRequest DeleteAssociation where
        type Sv DeleteAssociation = SSM
        type Rs DeleteAssociation = DeleteAssociationResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteAssociationResponse' <$> (pure (fromEnum s)))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delStatus'
newtype DeleteAssociationResponse = DeleteAssociationResponse'
    { _delStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAssociationResponse' smart constructor.
deleteAssociationResponse :: Int -> DeleteAssociationResponse
deleteAssociationResponse pStatus =
    DeleteAssociationResponse'
    { _delStatus = pStatus
    }

-- | FIXME: Undocumented member.
delStatus :: Lens' DeleteAssociationResponse Int
delStatus = lens _delStatus (\ s a -> s{_delStatus = a});
