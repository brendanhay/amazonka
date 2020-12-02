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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a gateway's metadata, which includes the gateway's name and time zone. To specify which gateway to update, use the Amazon Resource Name (ARN) of the gateway in your request.
--
--
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
    , ugirsGatewayName
    , ugirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'updateGatewayInformation' smart constructor.
data UpdateGatewayInformation = UpdateGatewayInformation'
  { _ugiGatewayName     :: !(Maybe Text)
  , _ugiGatewayTimezone :: !(Maybe Text)
  , _ugiGatewayARN      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGatewayInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugiGatewayName' - Undocumented member.
--
-- * 'ugiGatewayTimezone' - Undocumented member.
--
-- * 'ugiGatewayARN' - Undocumented member.
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
ugiGatewayName = lens _ugiGatewayName (\ s a -> s{_ugiGatewayName = a})

-- | Undocumented member.
ugiGatewayTimezone :: Lens' UpdateGatewayInformation (Maybe Text)
ugiGatewayTimezone = lens _ugiGatewayTimezone (\ s a -> s{_ugiGatewayTimezone = a})

-- | Undocumented member.
ugiGatewayARN :: Lens' UpdateGatewayInformation Text
ugiGatewayARN = lens _ugiGatewayARN (\ s a -> s{_ugiGatewayARN = a})

instance AWSRequest UpdateGatewayInformation where
        type Rs UpdateGatewayInformation =
             UpdateGatewayInformationResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGatewayInformationResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "GatewayName") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateGatewayInformation where

instance NFData UpdateGatewayInformation where

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

-- | A JSON object containing the ARN of the gateway that was updated.
--
--
--
-- /See:/ 'updateGatewayInformationResponse' smart constructor.
data UpdateGatewayInformationResponse = UpdateGatewayInformationResponse'
  { _ugirsGatewayARN     :: !(Maybe Text)
  , _ugirsGatewayName    :: !(Maybe Text)
  , _ugirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGatewayInformationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugirsGatewayARN' - Undocumented member.
--
-- * 'ugirsGatewayName' - Undocumented member.
--
-- * 'ugirsResponseStatus' - -- | The response status code.
updateGatewayInformationResponse
    :: Int -- ^ 'ugirsResponseStatus'
    -> UpdateGatewayInformationResponse
updateGatewayInformationResponse pResponseStatus_ =
  UpdateGatewayInformationResponse'
    { _ugirsGatewayARN = Nothing
    , _ugirsGatewayName = Nothing
    , _ugirsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ugirsGatewayARN :: Lens' UpdateGatewayInformationResponse (Maybe Text)
ugirsGatewayARN = lens _ugirsGatewayARN (\ s a -> s{_ugirsGatewayARN = a})

-- | Undocumented member.
ugirsGatewayName :: Lens' UpdateGatewayInformationResponse (Maybe Text)
ugirsGatewayName = lens _ugirsGatewayName (\ s a -> s{_ugirsGatewayName = a})

-- | -- | The response status code.
ugirsResponseStatus :: Lens' UpdateGatewayInformationResponse Int
ugirsResponseStatus = lens _ugirsResponseStatus (\ s a -> s{_ugirsResponseStatus = a})

instance NFData UpdateGatewayInformationResponse
         where
