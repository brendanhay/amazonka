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
-- Module      : Network.AWS.GameLift.UpdateFleetAttributes
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates fleet properties, including name and description, for a fleet. To update metadata, specify the fleet ID and the property values you want to change. If successful, the fleet ID for the updated fleet is returned.
module Network.AWS.GameLift.UpdateFleetAttributes
    (
    -- * Creating a Request
      updateFleetAttributes
    , UpdateFleetAttributes
    -- * Request Lenses
    , ufaNewGameSessionProtectionPolicy
    , ufaName
    , ufaDescription
    , ufaFleetId

    -- * Destructuring the Response
    , updateFleetAttributesResponse
    , UpdateFleetAttributesResponse
    -- * Response Lenses
    , ufarsFleetId
    , ufarsResponseStatus
    ) where

import           Network.AWS.GameLift.Types
import           Network.AWS.GameLift.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input for a request action.
--
-- /See:/ 'updateFleetAttributes' smart constructor.
data UpdateFleetAttributes = UpdateFleetAttributes'
    { _ufaNewGameSessionProtectionPolicy :: !(Maybe ProtectionPolicy)
    , _ufaName                           :: !(Maybe Text)
    , _ufaDescription                    :: !(Maybe Text)
    , _ufaFleetId                        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateFleetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufaNewGameSessionProtectionPolicy'
--
-- * 'ufaName'
--
-- * 'ufaDescription'
--
-- * 'ufaFleetId'
updateFleetAttributes
    :: Text -- ^ 'ufaFleetId'
    -> UpdateFleetAttributes
updateFleetAttributes pFleetId_ =
    UpdateFleetAttributes'
    { _ufaNewGameSessionProtectionPolicy = Nothing
    , _ufaName = Nothing
    , _ufaDescription = Nothing
    , _ufaFleetId = pFleetId_
    }

-- | Game session protection policy to apply to all new instances created in this fleet. Instances that already exist will not be affected. You can set protection for individual instances using < UpdateGameSession>.
--
-- -   NoProtection: The game session can be terminated during a scale-down event.
-- -   FullProtection: If the game session is in an ACTIVE status, it cannot be terminated during a scale-down event.
ufaNewGameSessionProtectionPolicy :: Lens' UpdateFleetAttributes (Maybe ProtectionPolicy)
ufaNewGameSessionProtectionPolicy = lens _ufaNewGameSessionProtectionPolicy (\ s a -> s{_ufaNewGameSessionProtectionPolicy = a});

-- | Descriptive label associated with this fleet. Fleet names do not need to be unique.
ufaName :: Lens' UpdateFleetAttributes (Maybe Text)
ufaName = lens _ufaName (\ s a -> s{_ufaName = a});

-- | Human-readable description of the fleet.
ufaDescription :: Lens' UpdateFleetAttributes (Maybe Text)
ufaDescription = lens _ufaDescription (\ s a -> s{_ufaDescription = a});

-- | Unique identifier for the fleet you want to update attribute metadata for.
ufaFleetId :: Lens' UpdateFleetAttributes Text
ufaFleetId = lens _ufaFleetId (\ s a -> s{_ufaFleetId = a});

instance AWSRequest UpdateFleetAttributes where
        type Rs UpdateFleetAttributes =
             UpdateFleetAttributesResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 UpdateFleetAttributesResponse' <$>
                   (x .?> "FleetId") <*> (pure (fromEnum s)))

instance Hashable UpdateFleetAttributes

instance NFData UpdateFleetAttributes

instance ToHeaders UpdateFleetAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.UpdateFleetAttributes" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateFleetAttributes where
        toJSON UpdateFleetAttributes'{..}
          = object
              (catMaybes
                 [("NewGameSessionProtectionPolicy" .=) <$>
                    _ufaNewGameSessionProtectionPolicy,
                  ("Name" .=) <$> _ufaName,
                  ("Description" .=) <$> _ufaDescription,
                  Just ("FleetId" .= _ufaFleetId)])

instance ToPath UpdateFleetAttributes where
        toPath = const "/"

instance ToQuery UpdateFleetAttributes where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
-- /See:/ 'updateFleetAttributesResponse' smart constructor.
data UpdateFleetAttributesResponse = UpdateFleetAttributesResponse'
    { _ufarsFleetId        :: !(Maybe Text)
    , _ufarsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateFleetAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufarsFleetId'
--
-- * 'ufarsResponseStatus'
updateFleetAttributesResponse
    :: Int -- ^ 'ufarsResponseStatus'
    -> UpdateFleetAttributesResponse
updateFleetAttributesResponse pResponseStatus_ =
    UpdateFleetAttributesResponse'
    { _ufarsFleetId = Nothing
    , _ufarsResponseStatus = pResponseStatus_
    }

-- | Unique identifier for the updated fleet.
ufarsFleetId :: Lens' UpdateFleetAttributesResponse (Maybe Text)
ufarsFleetId = lens _ufarsFleetId (\ s a -> s{_ufarsFleetId = a});

-- | The response status code.
ufarsResponseStatus :: Lens' UpdateFleetAttributesResponse Int
ufarsResponseStatus = lens _ufarsResponseStatus (\ s a -> s{_ufarsResponseStatus = a});

instance NFData UpdateFleetAttributesResponse
