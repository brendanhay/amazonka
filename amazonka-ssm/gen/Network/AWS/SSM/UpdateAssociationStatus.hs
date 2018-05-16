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
-- Module      : Network.AWS.SSM.UpdateAssociationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the Systems Manager document associated with the specified instance.
--
--
module Network.AWS.SSM.UpdateAssociationStatus
    (
    -- * Creating a Request
      updateAssociationStatus
    , UpdateAssociationStatus
    -- * Request Lenses
    , uasName
    , uasInstanceId
    , uasAssociationStatus

    -- * Destructuring the Response
    , updateAssociationStatusResponse
    , UpdateAssociationStatusResponse
    -- * Response Lenses
    , uasrsAssociationDescription
    , uasrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'updateAssociationStatus' smart constructor.
data UpdateAssociationStatus = UpdateAssociationStatus'
  { _uasName              :: !Text
  , _uasInstanceId        :: !Text
  , _uasAssociationStatus :: !AssociationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAssociationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasName' - The name of the Systems Manager document.
--
-- * 'uasInstanceId' - The ID of the instance.
--
-- * 'uasAssociationStatus' - The association status.
updateAssociationStatus
    :: Text -- ^ 'uasName'
    -> Text -- ^ 'uasInstanceId'
    -> AssociationStatus -- ^ 'uasAssociationStatus'
    -> UpdateAssociationStatus
updateAssociationStatus pName_ pInstanceId_ pAssociationStatus_ =
  UpdateAssociationStatus'
    { _uasName = pName_
    , _uasInstanceId = pInstanceId_
    , _uasAssociationStatus = pAssociationStatus_
    }


-- | The name of the Systems Manager document.
uasName :: Lens' UpdateAssociationStatus Text
uasName = lens _uasName (\ s a -> s{_uasName = a})

-- | The ID of the instance.
uasInstanceId :: Lens' UpdateAssociationStatus Text
uasInstanceId = lens _uasInstanceId (\ s a -> s{_uasInstanceId = a})

-- | The association status.
uasAssociationStatus :: Lens' UpdateAssociationStatus AssociationStatus
uasAssociationStatus = lens _uasAssociationStatus (\ s a -> s{_uasAssociationStatus = a})

instance AWSRequest UpdateAssociationStatus where
        type Rs UpdateAssociationStatus =
             UpdateAssociationStatusResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAssociationStatusResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateAssociationStatus where

instance NFData UpdateAssociationStatus where

instance ToHeaders UpdateAssociationStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.UpdateAssociationStatus" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAssociationStatus where
        toJSON UpdateAssociationStatus'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _uasName),
                  Just ("InstanceId" .= _uasInstanceId),
                  Just ("AssociationStatus" .= _uasAssociationStatus)])

instance ToPath UpdateAssociationStatus where
        toPath = const "/"

instance ToQuery UpdateAssociationStatus where
        toQuery = const mempty

-- | /See:/ 'updateAssociationStatusResponse' smart constructor.
data UpdateAssociationStatusResponse = UpdateAssociationStatusResponse'
  { _uasrsAssociationDescription :: !(Maybe AssociationDescription)
  , _uasrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAssociationStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasrsAssociationDescription' - Information about the association.
--
-- * 'uasrsResponseStatus' - -- | The response status code.
updateAssociationStatusResponse
    :: Int -- ^ 'uasrsResponseStatus'
    -> UpdateAssociationStatusResponse
updateAssociationStatusResponse pResponseStatus_ =
  UpdateAssociationStatusResponse'
    { _uasrsAssociationDescription = Nothing
    , _uasrsResponseStatus = pResponseStatus_
    }


-- | Information about the association.
uasrsAssociationDescription :: Lens' UpdateAssociationStatusResponse (Maybe AssociationDescription)
uasrsAssociationDescription = lens _uasrsAssociationDescription (\ s a -> s{_uasrsAssociationDescription = a})

-- | -- | The response status code.
uasrsResponseStatus :: Lens' UpdateAssociationStatusResponse Int
uasrsResponseStatus = lens _uasrsResponseStatus (\ s a -> s{_uasrsResponseStatus = a})

instance NFData UpdateAssociationStatusResponse where
