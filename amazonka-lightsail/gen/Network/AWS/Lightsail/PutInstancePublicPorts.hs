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
-- Module      : Network.AWS.Lightsail.PutInstancePublicPorts
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified open ports for an Amazon Lightsail instance, and closes all ports for every protocol not included in the current request.
--
--
module Network.AWS.Lightsail.PutInstancePublicPorts
    (
    -- * Creating a Request
      putInstancePublicPorts
    , PutInstancePublicPorts
    -- * Request Lenses
    , pippPortInfos
    , pippInstanceName

    -- * Destructuring the Response
    , putInstancePublicPortsResponse
    , PutInstancePublicPortsResponse
    -- * Response Lenses
    , pipprsOperation
    , pipprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putInstancePublicPorts' smart constructor.
data PutInstancePublicPorts = PutInstancePublicPorts'
  { _pippPortInfos    :: ![PortInfo]
  , _pippInstanceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutInstancePublicPorts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pippPortInfos' - Specifies information about the public port(s).
--
-- * 'pippInstanceName' - The Lightsail instance name of the public port(s) you are setting.
putInstancePublicPorts
    :: Text -- ^ 'pippInstanceName'
    -> PutInstancePublicPorts
putInstancePublicPorts pInstanceName_ =
  PutInstancePublicPorts'
    {_pippPortInfos = mempty, _pippInstanceName = pInstanceName_}


-- | Specifies information about the public port(s).
pippPortInfos :: Lens' PutInstancePublicPorts [PortInfo]
pippPortInfos = lens _pippPortInfos (\ s a -> s{_pippPortInfos = a}) . _Coerce

-- | The Lightsail instance name of the public port(s) you are setting.
pippInstanceName :: Lens' PutInstancePublicPorts Text
pippInstanceName = lens _pippInstanceName (\ s a -> s{_pippInstanceName = a})

instance AWSRequest PutInstancePublicPorts where
        type Rs PutInstancePublicPorts =
             PutInstancePublicPortsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 PutInstancePublicPortsResponse' <$>
                   (x .?> "operation") <*> (pure (fromEnum s)))

instance Hashable PutInstancePublicPorts where

instance NFData PutInstancePublicPorts where

instance ToHeaders PutInstancePublicPorts where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.PutInstancePublicPorts" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutInstancePublicPorts where
        toJSON PutInstancePublicPorts'{..}
          = object
              (catMaybes
                 [Just ("portInfos" .= _pippPortInfos),
                  Just ("instanceName" .= _pippInstanceName)])

instance ToPath PutInstancePublicPorts where
        toPath = const "/"

instance ToQuery PutInstancePublicPorts where
        toQuery = const mempty

-- | /See:/ 'putInstancePublicPortsResponse' smart constructor.
data PutInstancePublicPortsResponse = PutInstancePublicPortsResponse'
  { _pipprsOperation      :: !(Maybe Operation)
  , _pipprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutInstancePublicPortsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pipprsOperation' - Describes metadata about the operation you just executed.
--
-- * 'pipprsResponseStatus' - -- | The response status code.
putInstancePublicPortsResponse
    :: Int -- ^ 'pipprsResponseStatus'
    -> PutInstancePublicPortsResponse
putInstancePublicPortsResponse pResponseStatus_ =
  PutInstancePublicPortsResponse'
    {_pipprsOperation = Nothing, _pipprsResponseStatus = pResponseStatus_}


-- | Describes metadata about the operation you just executed.
pipprsOperation :: Lens' PutInstancePublicPortsResponse (Maybe Operation)
pipprsOperation = lens _pipprsOperation (\ s a -> s{_pipprsOperation = a})

-- | -- | The response status code.
pipprsResponseStatus :: Lens' PutInstancePublicPortsResponse Int
pipprsResponseStatus = lens _pipprsResponseStatus (\ s a -> s{_pipprsResponseStatus = a})

instance NFData PutInstancePublicPortsResponse where
