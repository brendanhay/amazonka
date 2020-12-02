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
-- Module      : Network.AWS.AppStream.AssociateFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified fleet with the specified stack.
--
--
module Network.AWS.AppStream.AssociateFleet
    (
    -- * Creating a Request
      associateFleet
    , AssociateFleet
    -- * Request Lenses
    , afFleetName
    , afStackName

    -- * Destructuring the Response
    , associateFleetResponse
    , AssociateFleetResponse
    -- * Response Lenses
    , afrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateFleet' smart constructor.
data AssociateFleet = AssociateFleet'
  { _afFleetName :: !Text
  , _afStackName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afFleetName' - The name of the fleet.
--
-- * 'afStackName' - The name of the stack.
associateFleet
    :: Text -- ^ 'afFleetName'
    -> Text -- ^ 'afStackName'
    -> AssociateFleet
associateFleet pFleetName_ pStackName_ =
  AssociateFleet' {_afFleetName = pFleetName_, _afStackName = pStackName_}


-- | The name of the fleet.
afFleetName :: Lens' AssociateFleet Text
afFleetName = lens _afFleetName (\ s a -> s{_afFleetName = a})

-- | The name of the stack.
afStackName :: Lens' AssociateFleet Text
afStackName = lens _afStackName (\ s a -> s{_afStackName = a})

instance AWSRequest AssociateFleet where
        type Rs AssociateFleet = AssociateFleetResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateFleetResponse' <$> (pure (fromEnum s)))

instance Hashable AssociateFleet where

instance NFData AssociateFleet where

instance ToHeaders AssociateFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.AssociateFleet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateFleet where
        toJSON AssociateFleet'{..}
          = object
              (catMaybes
                 [Just ("FleetName" .= _afFleetName),
                  Just ("StackName" .= _afStackName)])

instance ToPath AssociateFleet where
        toPath = const "/"

instance ToQuery AssociateFleet where
        toQuery = const mempty

-- | /See:/ 'associateFleetResponse' smart constructor.
newtype AssociateFleetResponse = AssociateFleetResponse'
  { _afrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afrsResponseStatus' - -- | The response status code.
associateFleetResponse
    :: Int -- ^ 'afrsResponseStatus'
    -> AssociateFleetResponse
associateFleetResponse pResponseStatus_ =
  AssociateFleetResponse' {_afrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
afrsResponseStatus :: Lens' AssociateFleetResponse Int
afrsResponseStatus = lens _afrsResponseStatus (\ s a -> s{_afrsResponseStatus = a})

instance NFData AssociateFleetResponse where
