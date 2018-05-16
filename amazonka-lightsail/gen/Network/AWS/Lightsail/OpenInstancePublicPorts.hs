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
-- Module      : Network.AWS.Lightsail.OpenInstancePublicPorts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds public ports to an Amazon Lightsail instance.
--
--
module Network.AWS.Lightsail.OpenInstancePublicPorts
    (
    -- * Creating a Request
      openInstancePublicPorts
    , OpenInstancePublicPorts
    -- * Request Lenses
    , oippPortInfo
    , oippInstanceName

    -- * Destructuring the Response
    , openInstancePublicPortsResponse
    , OpenInstancePublicPortsResponse
    -- * Response Lenses
    , oipprsOperation
    , oipprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'openInstancePublicPorts' smart constructor.
data OpenInstancePublicPorts = OpenInstancePublicPorts'
  { _oippPortInfo     :: !PortInfo
  , _oippInstanceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OpenInstancePublicPorts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oippPortInfo' - An array of key-value pairs containing information about the port mappings.
--
-- * 'oippInstanceName' - The name of the instance for which you want to open the public ports.
openInstancePublicPorts
    :: PortInfo -- ^ 'oippPortInfo'
    -> Text -- ^ 'oippInstanceName'
    -> OpenInstancePublicPorts
openInstancePublicPorts pPortInfo_ pInstanceName_ =
  OpenInstancePublicPorts'
    {_oippPortInfo = pPortInfo_, _oippInstanceName = pInstanceName_}


-- | An array of key-value pairs containing information about the port mappings.
oippPortInfo :: Lens' OpenInstancePublicPorts PortInfo
oippPortInfo = lens _oippPortInfo (\ s a -> s{_oippPortInfo = a})

-- | The name of the instance for which you want to open the public ports.
oippInstanceName :: Lens' OpenInstancePublicPorts Text
oippInstanceName = lens _oippInstanceName (\ s a -> s{_oippInstanceName = a})

instance AWSRequest OpenInstancePublicPorts where
        type Rs OpenInstancePublicPorts =
             OpenInstancePublicPortsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 OpenInstancePublicPortsResponse' <$>
                   (x .?> "operation") <*> (pure (fromEnum s)))

instance Hashable OpenInstancePublicPorts where

instance NFData OpenInstancePublicPorts where

instance ToHeaders OpenInstancePublicPorts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.OpenInstancePublicPorts" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON OpenInstancePublicPorts where
        toJSON OpenInstancePublicPorts'{..}
          = object
              (catMaybes
                 [Just ("portInfo" .= _oippPortInfo),
                  Just ("instanceName" .= _oippInstanceName)])

instance ToPath OpenInstancePublicPorts where
        toPath = const "/"

instance ToQuery OpenInstancePublicPorts where
        toQuery = const mempty

-- | /See:/ 'openInstancePublicPortsResponse' smart constructor.
data OpenInstancePublicPortsResponse = OpenInstancePublicPortsResponse'
  { _oipprsOperation      :: !(Maybe Operation)
  , _oipprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OpenInstancePublicPortsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oipprsOperation' - An array of key-value pairs containing information about the request operation.
--
-- * 'oipprsResponseStatus' - -- | The response status code.
openInstancePublicPortsResponse
    :: Int -- ^ 'oipprsResponseStatus'
    -> OpenInstancePublicPortsResponse
openInstancePublicPortsResponse pResponseStatus_ =
  OpenInstancePublicPortsResponse'
    {_oipprsOperation = Nothing, _oipprsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the request operation.
oipprsOperation :: Lens' OpenInstancePublicPortsResponse (Maybe Operation)
oipprsOperation = lens _oipprsOperation (\ s a -> s{_oipprsOperation = a})

-- | -- | The response status code.
oipprsResponseStatus :: Lens' OpenInstancePublicPortsResponse Int
oipprsResponseStatus = lens _oipprsResponseStatus (\ s a -> s{_oipprsResponseStatus = a})

instance NFData OpenInstancePublicPortsResponse where
