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
    , uasrqName
    , uasrqInstanceId
    , uasrqAssociationStatus

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
-- * 'uasrqName'
--
-- * 'uasrqInstanceId'
--
-- * 'uasrqAssociationStatus'
data UpdateAssociationStatus = UpdateAssociationStatus'
    { _uasrqName              :: !Text
    , _uasrqInstanceId        :: !Text
    , _uasrqAssociationStatus :: !AssociationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAssociationStatus' smart constructor.
updateAssociationStatus :: Text -> Text -> AssociationStatus -> UpdateAssociationStatus
updateAssociationStatus pName pInstanceId pAssociationStatus =
    UpdateAssociationStatus'
    { _uasrqName = pName
    , _uasrqInstanceId = pInstanceId
    , _uasrqAssociationStatus = pAssociationStatus
    }

-- | The name of the configuration document.
uasrqName :: Lens' UpdateAssociationStatus Text
uasrqName = lens _uasrqName (\ s a -> s{_uasrqName = a});

-- | The ID of the instance.
uasrqInstanceId :: Lens' UpdateAssociationStatus Text
uasrqInstanceId = lens _uasrqInstanceId (\ s a -> s{_uasrqInstanceId = a});

-- | The association status.
uasrqAssociationStatus :: Lens' UpdateAssociationStatus AssociationStatus
uasrqAssociationStatus = lens _uasrqAssociationStatus (\ s a -> s{_uasrqAssociationStatus = a});

instance AWSRequest UpdateAssociationStatus where
        type Sv UpdateAssociationStatus = SSM
        type Rs UpdateAssociationStatus =
             UpdateAssociationStatusResponse
        request = postJSON
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
              ["Name" .= _uasrqName,
               "InstanceId" .= _uasrqInstanceId,
               "AssociationStatus" .= _uasrqAssociationStatus]

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
updateAssociationStatusResponse pStatus =
    UpdateAssociationStatusResponse'
    { _uasrsAssociationDescription = Nothing
    , _uasrsStatus = pStatus
    }

-- | Information about the association.
uasrsAssociationDescription :: Lens' UpdateAssociationStatusResponse (Maybe AssociationDescription)
uasrsAssociationDescription = lens _uasrsAssociationDescription (\ s a -> s{_uasrsAssociationDescription = a});

-- | FIXME: Undocumented member.
uasrsStatus :: Lens' UpdateAssociationStatusResponse Int
uasrsStatus = lens _uasrsStatus (\ s a -> s{_uasrsStatus = a});
