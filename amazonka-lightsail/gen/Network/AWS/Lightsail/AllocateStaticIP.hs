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
-- Module      : Network.AWS.Lightsail.AllocateStaticIP
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a static IP address.
--
--
module Network.AWS.Lightsail.AllocateStaticIP
    (
    -- * Creating a Request
      allocateStaticIP
    , AllocateStaticIP
    -- * Request Lenses
    , asiStaticIPName

    -- * Destructuring the Response
    , allocateStaticIPResponse
    , AllocateStaticIPResponse
    -- * Response Lenses
    , asirsOperations
    , asirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'allocateStaticIP' smart constructor.
newtype AllocateStaticIP = AllocateStaticIP'
  { _asiStaticIPName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllocateStaticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asiStaticIPName' - The name of the static IP address.
allocateStaticIP
    :: Text -- ^ 'asiStaticIPName'
    -> AllocateStaticIP
allocateStaticIP pStaticIPName_ =
  AllocateStaticIP' {_asiStaticIPName = pStaticIPName_}


-- | The name of the static IP address.
asiStaticIPName :: Lens' AllocateStaticIP Text
asiStaticIPName = lens _asiStaticIPName (\ s a -> s{_asiStaticIPName = a})

instance AWSRequest AllocateStaticIP where
        type Rs AllocateStaticIP = AllocateStaticIPResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 AllocateStaticIPResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable AllocateStaticIP where

instance NFData AllocateStaticIP where

instance ToHeaders AllocateStaticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.AllocateStaticIp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AllocateStaticIP where
        toJSON AllocateStaticIP'{..}
          = object
              (catMaybes
                 [Just ("staticIpName" .= _asiStaticIPName)])

instance ToPath AllocateStaticIP where
        toPath = const "/"

instance ToQuery AllocateStaticIP where
        toQuery = const mempty

-- | /See:/ 'allocateStaticIPResponse' smart constructor.
data AllocateStaticIPResponse = AllocateStaticIPResponse'
  { _asirsOperations     :: !(Maybe [Operation])
  , _asirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllocateStaticIPResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asirsOperations' - An array of key-value pairs containing information about the static IP address you allocated.
--
-- * 'asirsResponseStatus' - -- | The response status code.
allocateStaticIPResponse
    :: Int -- ^ 'asirsResponseStatus'
    -> AllocateStaticIPResponse
allocateStaticIPResponse pResponseStatus_ =
  AllocateStaticIPResponse'
    {_asirsOperations = Nothing, _asirsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the static IP address you allocated.
asirsOperations :: Lens' AllocateStaticIPResponse [Operation]
asirsOperations = lens _asirsOperations (\ s a -> s{_asirsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
asirsResponseStatus :: Lens' AllocateStaticIPResponse Int
asirsResponseStatus = lens _asirsResponseStatus (\ s a -> s{_asirsResponseStatus = a})

instance NFData AllocateStaticIPResponse where
