{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified configuration document from the specified
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
    , drqName
    , drqInstanceId

    -- * Response
    , DeleteAssociationResponse
    -- ** Response constructor
    , deleteAssociationResponse
    -- ** Response lenses
    , delrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'deleteAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqName'
--
-- * 'drqInstanceId'
data DeleteAssociation = DeleteAssociation'
    { _drqName       :: !Text
    , _drqInstanceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAssociation' smart constructor.
deleteAssociation :: Text -> Text -> DeleteAssociation
deleteAssociation pName_ pInstanceId_ =
    DeleteAssociation'
    { _drqName = pName_
    , _drqInstanceId = pInstanceId_
    }

-- | The name of the configuration document.
drqName :: Lens' DeleteAssociation Text
drqName = lens _drqName (\ s a -> s{_drqName = a});

-- | The ID of the instance.
drqInstanceId :: Lens' DeleteAssociation Text
drqInstanceId = lens _drqInstanceId (\ s a -> s{_drqInstanceId = a});

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
              ["Name" .= _drqName, "InstanceId" .= _drqInstanceId]

instance ToPath DeleteAssociation where
        toPath = const "/"

instance ToQuery DeleteAssociation where
        toQuery = const mempty

-- | /See:/ 'deleteAssociationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delrsStatus'
newtype DeleteAssociationResponse = DeleteAssociationResponse'
    { _delrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAssociationResponse' smart constructor.
deleteAssociationResponse :: Int -> DeleteAssociationResponse
deleteAssociationResponse pStatus_ =
    DeleteAssociationResponse'
    { _delrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
delrsStatus :: Lens' DeleteAssociationResponse Int
delrsStatus = lens _delrsStatus (\ s a -> s{_delrsStatus = a});
