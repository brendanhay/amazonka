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
-- Module      : Network.AWS.StorageGateway.UpdateGatewayInformation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates a gateway\'s metadata, which includes the
-- gateway\'s name and time zone. To specify which gateway to update, use
-- the Amazon Resource Name (ARN) of the gateway in your request.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateGatewayInformation.html AWS API Reference> for UpdateGatewayInformation.
module Network.AWS.StorageGateway.UpdateGatewayInformation
    (
    -- * Creating a Request
      updateGatewayInformation
    , UpdateGatewayInformation
    -- * Request Lenses
    , ugiGatewayName
    , ugiGatewayTimezone
    , ugiGatewayARN

    -- * Destructuring the Response
    , updateGatewayInformationResponse
    , UpdateGatewayInformationResponse
    -- * Response Lenses
    , ugirsGatewayARN
    , ugirsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'updateGatewayInformation' smart constructor.
data UpdateGatewayInformation = UpdateGatewayInformation'
    { _ugiGatewayName     :: !(Maybe Text)
    , _ugiGatewayTimezone :: !(Maybe Text)
    , _ugiGatewayARN      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateGatewayInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugiGatewayName'
--
-- * 'ugiGatewayTimezone'
--
-- * 'ugiGatewayARN'
updateGatewayInformation
    :: Text -- ^ 'ugiGatewayARN'
    -> UpdateGatewayInformation
updateGatewayInformation pGatewayARN_ =
    UpdateGatewayInformation'
    { _ugiGatewayName = Nothing
    , _ugiGatewayTimezone = Nothing
    , _ugiGatewayARN = pGatewayARN_
    }

-- | Undocumented member.
ugiGatewayName :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayName = lens _ugiGatewayName (\ s a -> s{_ugiGatewayName = a});

-- | Undocumented member.
ugiGatewayTimezone :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayTimezone = lens _ugiGatewayTimezone (\ s a -> s{_ugiGatewayTimezone = a});

-- | Undocumented member.
ugiGatewayARN :: Lens' UpdateGatewayInformation Text
ugiGatewayARN = lens _ugiGatewayARN (\ s a -> s{_ugiGatewayARN = a});

instance AWSRequest UpdateGatewayInformation where
        type Rs UpdateGatewayInformation =
             UpdateGatewayInformationResponse
        request = postJSON storageGateway
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
              (catMaybes
                 [("GatewayName" .=) <$> _ugiGatewayName,
                  ("GatewayTimezone" .=) <$> _ugiGatewayTimezone,
                  Just ("GatewayARN" .= _ugiGatewayARN)])

instance ToPath UpdateGatewayInformation where
        toPath = const "/"

instance ToQuery UpdateGatewayInformation where
        toQuery = const mempty

-- | A JSON object containing the of the gateway that was updated.
--
-- /See:/ 'updateGatewayInformationResponse' smart constructor.
data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse'
    { _ugirsGatewayARN :: !(Maybe Text)
    , _ugirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateGatewayInformationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugirsGatewayARN'
--
-- * 'ugirsStatus'
updateGatewayInformationResponse
    :: Int -- ^ 'ugirsStatus'
    -> UpdateGatewayInformationResponse
updateGatewayInformationResponse pStatus_ =
    UpdateGatewayInformationResponse'
    { _ugirsGatewayARN = Nothing
    , _ugirsStatus = pStatus_
    }

-- | Undocumented member.
ugirsGatewayARN :: Lens' UpdateGatewayInformationResponse (Maybe Text)
ugirsGatewayARN = lens _ugirsGatewayARN (\ s a -> s{_ugirsGatewayARN = a});

-- | The response status code.
ugirsStatus :: Lens' UpdateGatewayInformationResponse Int
ugirsStatus = lens _ugirsStatus (\ s a -> s{_ugirsStatus = a});
