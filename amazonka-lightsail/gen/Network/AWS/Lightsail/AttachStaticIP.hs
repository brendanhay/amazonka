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
-- Module      : Network.AWS.Lightsail.AttachStaticIP
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a static IP address to a specific Amazon Lightsail instance.
--
--
module Network.AWS.Lightsail.AttachStaticIP
    (
    -- * Creating a Request
      attachStaticIP
    , AttachStaticIP
    -- * Request Lenses
    , asipStaticIPName
    , asipInstanceName

    -- * Destructuring the Response
    , attachStaticIPResponse
    , AttachStaticIPResponse
    -- * Response Lenses
    , asiprsOperations
    , asiprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachStaticIP' smart constructor.
data AttachStaticIP = AttachStaticIP'
  { _asipStaticIPName :: !Text
  , _asipInstanceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachStaticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asipStaticIPName' - The name of the static IP.
--
-- * 'asipInstanceName' - The instance name to which you want to attach the static IP address.
attachStaticIP
    :: Text -- ^ 'asipStaticIPName'
    -> Text -- ^ 'asipInstanceName'
    -> AttachStaticIP
attachStaticIP pStaticIPName_ pInstanceName_ =
  AttachStaticIP'
    {_asipStaticIPName = pStaticIPName_, _asipInstanceName = pInstanceName_}


-- | The name of the static IP.
asipStaticIPName :: Lens' AttachStaticIP Text
asipStaticIPName = lens _asipStaticIPName (\ s a -> s{_asipStaticIPName = a})

-- | The instance name to which you want to attach the static IP address.
asipInstanceName :: Lens' AttachStaticIP Text
asipInstanceName = lens _asipInstanceName (\ s a -> s{_asipInstanceName = a})

instance AWSRequest AttachStaticIP where
        type Rs AttachStaticIP = AttachStaticIPResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 AttachStaticIPResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable AttachStaticIP where

instance NFData AttachStaticIP where

instance ToHeaders AttachStaticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.AttachStaticIp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AttachStaticIP where
        toJSON AttachStaticIP'{..}
          = object
              (catMaybes
                 [Just ("staticIpName" .= _asipStaticIPName),
                  Just ("instanceName" .= _asipInstanceName)])

instance ToPath AttachStaticIP where
        toPath = const "/"

instance ToQuery AttachStaticIP where
        toQuery = const mempty

-- | /See:/ 'attachStaticIPResponse' smart constructor.
data AttachStaticIPResponse = AttachStaticIPResponse'
  { _asiprsOperations     :: !(Maybe [Operation])
  , _asiprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachStaticIPResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asiprsOperations' - An array of key-value pairs containing information about your API operations.
--
-- * 'asiprsResponseStatus' - -- | The response status code.
attachStaticIPResponse
    :: Int -- ^ 'asiprsResponseStatus'
    -> AttachStaticIPResponse
attachStaticIPResponse pResponseStatus_ =
  AttachStaticIPResponse'
    {_asiprsOperations = Nothing, _asiprsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about your API operations.
asiprsOperations :: Lens' AttachStaticIPResponse [Operation]
asiprsOperations = lens _asiprsOperations (\ s a -> s{_asiprsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
asiprsResponseStatus :: Lens' AttachStaticIPResponse Int
asiprsResponseStatus = lens _asiprsResponseStatus (\ s a -> s{_asiprsResponseStatus = a})

instance NFData AttachStaticIPResponse where
