{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/ssm/latest/APIReference/API_DeleteAssociation.html AWS API Reference> for DeleteAssociation.
module Network.AWS.SSM.DeleteAssociation
    (
    -- * Creating a Request
      deleteAssociation
    , DeleteAssociation
    -- * Request Lenses
    , delName
    , delInstanceId

    -- * Destructuring the Response
    , deleteAssociationResponse
    , DeleteAssociationResponse
    -- * Response Lenses
    , delrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'deleteAssociation' smart constructor.
data DeleteAssociation = DeleteAssociation'
    { _delName       :: !Text
    , _delInstanceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delName'
--
-- * 'delInstanceId'
deleteAssociation
    :: Text -- ^ 'delName'
    -> Text -- ^ 'delInstanceId'
    -> DeleteAssociation
deleteAssociation pName_ pInstanceId_ =
    DeleteAssociation'
    { _delName = pName_
    , _delInstanceId = pInstanceId_
    }

-- | The name of the configuration document.
delName :: Lens' DeleteAssociation Text
delName = lens _delName (\ s a -> s{_delName = a});

-- | The ID of the instance.
delInstanceId :: Lens' DeleteAssociation Text
delInstanceId = lens _delInstanceId (\ s a -> s{_delInstanceId = a});

instance AWSRequest DeleteAssociation where
        type Rs DeleteAssociation = DeleteAssociationResponse
        request = postJSON sSM
        response
          = receiveEmpty
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
              (catMaybes
                 [Just ("Name" .= _delName),
                  Just ("InstanceId" .= _delInstanceId)])

instance ToPath DeleteAssociation where
        toPath = const "/"

instance ToQuery DeleteAssociation where
        toQuery = const mempty

-- | /See:/ 'deleteAssociationResponse' smart constructor.
newtype DeleteAssociationResponse = DeleteAssociationResponse'
    { _delrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsStatus'
deleteAssociationResponse
    :: Int -- ^ 'delrsStatus'
    -> DeleteAssociationResponse
deleteAssociationResponse pStatus_ =
    DeleteAssociationResponse'
    { _delrsStatus = pStatus_
    }

-- | The response status code.
delrsStatus :: Lens' DeleteAssociationResponse Int
delrsStatus = lens _delrsStatus (\ s a -> s{_delrsStatus = a});
