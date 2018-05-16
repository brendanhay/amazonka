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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified Systems Manager document from the specified instance.
--
--
-- When you disassociate a document from an instance, it does not change the configuration of the instance. To change the configuration state of an instance after you disassociate a document, you must create a new document with the desired configuration and associate it with the instance.
--
module Network.AWS.SSM.DeleteAssociation
    (
    -- * Creating a Request
      deleteAssociation
    , DeleteAssociation
    -- * Request Lenses
    , daaAssociationId
    , daaInstanceId
    , daaName

    -- * Destructuring the Response
    , deleteAssociationResponse
    , DeleteAssociationResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deleteAssociation' smart constructor.
data DeleteAssociation = DeleteAssociation'
  { _daaAssociationId :: !(Maybe Text)
  , _daaInstanceId    :: !(Maybe Text)
  , _daaName          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daaAssociationId' - The association ID that you want to delete.
--
-- * 'daaInstanceId' - The ID of the instance.
--
-- * 'daaName' - The name of the Systems Manager document.
deleteAssociation
    :: DeleteAssociation
deleteAssociation =
  DeleteAssociation'
    {_daaAssociationId = Nothing, _daaInstanceId = Nothing, _daaName = Nothing}


-- | The association ID that you want to delete.
daaAssociationId :: Lens' DeleteAssociation (Maybe Text)
daaAssociationId = lens _daaAssociationId (\ s a -> s{_daaAssociationId = a})

-- | The ID of the instance.
daaInstanceId :: Lens' DeleteAssociation (Maybe Text)
daaInstanceId = lens _daaInstanceId (\ s a -> s{_daaInstanceId = a})

-- | The name of the Systems Manager document.
daaName :: Lens' DeleteAssociation (Maybe Text)
daaName = lens _daaName (\ s a -> s{_daaName = a})

instance AWSRequest DeleteAssociation where
        type Rs DeleteAssociation = DeleteAssociationResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteAssociationResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteAssociation where

instance NFData DeleteAssociation where

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
                 [("AssociationId" .=) <$> _daaAssociationId,
                  ("InstanceId" .=) <$> _daaInstanceId,
                  ("Name" .=) <$> _daaName])

instance ToPath DeleteAssociation where
        toPath = const "/"

instance ToQuery DeleteAssociation where
        toQuery = const mempty

-- | /See:/ 'deleteAssociationResponse' smart constructor.
newtype DeleteAssociationResponse = DeleteAssociationResponse'
  { _delrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteAssociationResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteAssociationResponse
deleteAssociationResponse pResponseStatus_ =
  DeleteAssociationResponse' {_delrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteAssociationResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteAssociationResponse where
