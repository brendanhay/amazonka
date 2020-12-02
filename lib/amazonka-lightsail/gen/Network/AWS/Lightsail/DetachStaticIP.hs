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
-- Module      : Network.AWS.Lightsail.DetachStaticIP
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a static IP from the Amazon Lightsail instance to which it is attached.
--
--
module Network.AWS.Lightsail.DetachStaticIP
    (
    -- * Creating a Request
      detachStaticIP
    , DetachStaticIP
    -- * Request Lenses
    , dsiStaticIPName

    -- * Destructuring the Response
    , detachStaticIPResponse
    , DetachStaticIPResponse
    -- * Response Lenses
    , dsirsOperations
    , dsirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachStaticIP' smart constructor.
newtype DetachStaticIP = DetachStaticIP'
  { _dsiStaticIPName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachStaticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsiStaticIPName' - The name of the static IP to detach from the instance.
detachStaticIP
    :: Text -- ^ 'dsiStaticIPName'
    -> DetachStaticIP
detachStaticIP pStaticIPName_ =
  DetachStaticIP' {_dsiStaticIPName = pStaticIPName_}


-- | The name of the static IP to detach from the instance.
dsiStaticIPName :: Lens' DetachStaticIP Text
dsiStaticIPName = lens _dsiStaticIPName (\ s a -> s{_dsiStaticIPName = a})

instance AWSRequest DetachStaticIP where
        type Rs DetachStaticIP = DetachStaticIPResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DetachStaticIPResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DetachStaticIP where

instance NFData DetachStaticIP where

instance ToHeaders DetachStaticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DetachStaticIp" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DetachStaticIP where
        toJSON DetachStaticIP'{..}
          = object
              (catMaybes
                 [Just ("staticIpName" .= _dsiStaticIPName)])

instance ToPath DetachStaticIP where
        toPath = const "/"

instance ToQuery DetachStaticIP where
        toQuery = const mempty

-- | /See:/ 'detachStaticIPResponse' smart constructor.
data DetachStaticIPResponse = DetachStaticIPResponse'
  { _dsirsOperations     :: !(Maybe [Operation])
  , _dsirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachStaticIPResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsirsOperations' - An array of key-value pairs containing information about the results of your detach static IP request.
--
-- * 'dsirsResponseStatus' - -- | The response status code.
detachStaticIPResponse
    :: Int -- ^ 'dsirsResponseStatus'
    -> DetachStaticIPResponse
detachStaticIPResponse pResponseStatus_ =
  DetachStaticIPResponse'
    {_dsirsOperations = Nothing, _dsirsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the results of your detach static IP request.
dsirsOperations :: Lens' DetachStaticIPResponse [Operation]
dsirsOperations = lens _dsirsOperations (\ s a -> s{_dsirsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
dsirsResponseStatus :: Lens' DetachStaticIPResponse Int
dsirsResponseStatus = lens _dsirsResponseStatus (\ s a -> s{_dsirsResponseStatus = a})

instance NFData DetachStaticIPResponse where
