{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateGatewayInformation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation updates a gateway\'s metadata, which includes the
-- gateway\'s name and time zone. To specify which gateway to update, use
-- the Amazon Resource Name (ARN) of the gateway in your request.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateGatewayInformation.html>
module Network.AWS.StorageGateway.UpdateGatewayInformation
    (
    -- * Request
      UpdateGatewayInformation
    -- ** Request constructor
    , updateGatewayInformation
    -- ** Request lenses
    , ugiGatewayName
    , ugiGatewayTimezone
    , ugiGatewayARN

    -- * Response
    , UpdateGatewayInformationResponse
    -- ** Response constructor
    , updateGatewayInformationResponse
    -- ** Response lenses
    , ugirsGatewayARN
    , ugirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | /See:/ 'updateGatewayInformation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugiGatewayName'
--
-- * 'ugiGatewayTimezone'
--
-- * 'ugiGatewayARN'
data UpdateGatewayInformation = UpdateGatewayInformation'
    { _ugiGatewayName     :: !(Maybe Text)
    , _ugiGatewayTimezone :: !(Maybe Text)
    , _ugiGatewayARN      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateGatewayInformation' smart constructor.
updateGatewayInformation :: Text -> UpdateGatewayInformation
updateGatewayInformation pGatewayARN_ =
    UpdateGatewayInformation'
    { _ugiGatewayName = Nothing
    , _ugiGatewayTimezone = Nothing
    , _ugiGatewayARN = pGatewayARN_
    }

-- | FIXME: Undocumented member.
ugiGatewayName :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayName = lens _ugiGatewayName (\ s a -> s{_ugiGatewayName = a});

-- | FIXME: Undocumented member.
ugiGatewayTimezone :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayTimezone = lens _ugiGatewayTimezone (\ s a -> s{_ugiGatewayTimezone = a});

-- | FIXME: Undocumented member.
ugiGatewayARN :: Lens' UpdateGatewayInformation Text
ugiGatewayARN = lens _ugiGatewayARN (\ s a -> s{_ugiGatewayARN = a});

instance AWSRequest UpdateGatewayInformation where
        type Sv UpdateGatewayInformation = StorageGateway
        type Rs UpdateGatewayInformation =
             UpdateGatewayInformationResponse
        request = postJSON "UpdateGatewayInformation"
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGatewayInformationResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance ToHeaders UpdateGatewayInformation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.UpdateGatewayInformation"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGatewayInformation where
        toJSON UpdateGatewayInformation'{..}
          = object
              ["GatewayName" .= _ugiGatewayName,
               "GatewayTimezone" .= _ugiGatewayTimezone,
               "GatewayARN" .= _ugiGatewayARN]

instance ToPath UpdateGatewayInformation where
        toPath = const "/"

instance ToQuery UpdateGatewayInformation where
        toQuery = const mempty

-- | A JSON object containing the of the gateway that was updated.
--
-- /See:/ 'updateGatewayInformationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugirsGatewayARN'
--
-- * 'ugirsStatus'
data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse'
    { _ugirsGatewayARN :: !(Maybe Text)
    , _ugirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateGatewayInformationResponse' smart constructor.
updateGatewayInformationResponse :: Int -> UpdateGatewayInformationResponse
updateGatewayInformationResponse pStatus_ =
    UpdateGatewayInformationResponse'
    { _ugirsGatewayARN = Nothing
    , _ugirsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
ugirsGatewayARN :: Lens' UpdateGatewayInformationResponse (Maybe Text)
ugirsGatewayARN = lens _ugirsGatewayARN (\ s a -> s{_ugirsGatewayARN = a});

-- | FIXME: Undocumented member.
ugirsStatus :: Lens' UpdateGatewayInformationResponse Int
ugirsStatus = lens _ugirsStatus (\ s a -> s{_ugirsStatus = a});
