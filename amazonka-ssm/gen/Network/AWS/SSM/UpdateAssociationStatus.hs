{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateAssociationStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the configuration document associated with the
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
    , uasName
    , uasInstanceId
    , uasAssociationStatus

    -- * Response
    , UpdateAssociationStatusResponse
    -- ** Response constructor
    , updateAssociationStatusResponse
    -- ** Response lenses
    , uasrsAssociationDescription
    , uasrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types

-- | /See:/ 'updateAssociationStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uasName'
--
-- * 'uasInstanceId'
--
-- * 'uasAssociationStatus'
data UpdateAssociationStatus = UpdateAssociationStatus'
    { _uasName              :: !Text
    , _uasInstanceId        :: !Text
    , _uasAssociationStatus :: !AssociationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAssociationStatus' smart constructor.
updateAssociationStatus :: Text -> Text -> AssociationStatus -> UpdateAssociationStatus
updateAssociationStatus pName_ pInstanceId_ pAssociationStatus_ =
    UpdateAssociationStatus'
    { _uasName = pName_
    , _uasInstanceId = pInstanceId_
    , _uasAssociationStatus = pAssociationStatus_
    }

-- | The name of the configuration document.
uasName :: Lens' UpdateAssociationStatus Text
uasName = lens _uasName (\ s a -> s{_uasName = a});

-- | The ID of the instance.
uasInstanceId :: Lens' UpdateAssociationStatus Text
uasInstanceId = lens _uasInstanceId (\ s a -> s{_uasInstanceId = a});

-- | The association status.
uasAssociationStatus :: Lens' UpdateAssociationStatus AssociationStatus
uasAssociationStatus = lens _uasAssociationStatus (\ s a -> s{_uasAssociationStatus = a});

instance AWSRequest UpdateAssociationStatus where
        type Sv UpdateAssociationStatus = SSM
        type Rs UpdateAssociationStatus =
             UpdateAssociationStatusResponse
        request = postJSON "UpdateAssociationStatus"
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAssociationStatusResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

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
              ["Name" .= _uasName, "InstanceId" .= _uasInstanceId,
               "AssociationStatus" .= _uasAssociationStatus]

instance ToPath UpdateAssociationStatus where
        toPath = const "/"

instance ToQuery UpdateAssociationStatus where
        toQuery = const mempty

-- | /See:/ 'updateAssociationStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uasrsAssociationDescription'
--
-- * 'uasrsStatus'
data UpdateAssociationStatusResponse = UpdateAssociationStatusResponse'
    { _uasrsAssociationDescription :: !(Maybe AssociationDescription)
    , _uasrsStatus                 :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAssociationStatusResponse' smart constructor.
updateAssociationStatusResponse :: Int -> UpdateAssociationStatusResponse
updateAssociationStatusResponse pStatus_ =
    UpdateAssociationStatusResponse'
    { _uasrsAssociationDescription = Nothing
    , _uasrsStatus = pStatus_
    }

-- | Information about the association.
uasrsAssociationDescription :: Lens' UpdateAssociationStatusResponse (Maybe AssociationDescription)
uasrsAssociationDescription = lens _uasrsAssociationDescription (\ s a -> s{_uasrsAssociationDescription = a});

-- | FIXME: Undocumented member.
uasrsStatus :: Lens' UpdateAssociationStatusResponse Int
uasrsStatus = lens _uasrsStatus (\ s a -> s{_uasrsStatus = a});
