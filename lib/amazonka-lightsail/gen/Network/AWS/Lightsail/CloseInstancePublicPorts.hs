{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CloseInstancePublicPorts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Closes ports for a specific Amazon Lightsail instance.
--
--
-- The @CloseInstancePublicPorts@ action supports tag-based access control via resource tags applied to the resource identified by @instanceName@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CloseInstancePublicPorts
  ( -- * Creating a Request
    closeInstancePublicPorts,
    CloseInstancePublicPorts,

    -- * Request Lenses
    cippPortInfo,
    cippInstanceName,

    -- * Destructuring the Response
    closeInstancePublicPortsResponse,
    CloseInstancePublicPortsResponse,

    -- * Response Lenses
    cipprsOperation,
    cipprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'closeInstancePublicPorts' smart constructor.
data CloseInstancePublicPorts = CloseInstancePublicPorts'
  { _cippPortInfo ::
      !PortInfo,
    _cippInstanceName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloseInstancePublicPorts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cippPortInfo' - An object to describe the ports to close for the specified instance.
--
-- * 'cippInstanceName' - The name of the instance for which to close ports.
closeInstancePublicPorts ::
  -- | 'cippPortInfo'
  PortInfo ->
  -- | 'cippInstanceName'
  Text ->
  CloseInstancePublicPorts
closeInstancePublicPorts pPortInfo_ pInstanceName_ =
  CloseInstancePublicPorts'
    { _cippPortInfo = pPortInfo_,
      _cippInstanceName = pInstanceName_
    }

-- | An object to describe the ports to close for the specified instance.
cippPortInfo :: Lens' CloseInstancePublicPorts PortInfo
cippPortInfo = lens _cippPortInfo (\s a -> s {_cippPortInfo = a})

-- | The name of the instance for which to close ports.
cippInstanceName :: Lens' CloseInstancePublicPorts Text
cippInstanceName = lens _cippInstanceName (\s a -> s {_cippInstanceName = a})

instance AWSRequest CloseInstancePublicPorts where
  type Rs CloseInstancePublicPorts = CloseInstancePublicPortsResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CloseInstancePublicPortsResponse'
            <$> (x .?> "operation") <*> (pure (fromEnum s))
      )

instance Hashable CloseInstancePublicPorts

instance NFData CloseInstancePublicPorts

instance ToHeaders CloseInstancePublicPorts where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.CloseInstancePublicPorts" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CloseInstancePublicPorts where
  toJSON CloseInstancePublicPorts' {..} =
    object
      ( catMaybes
          [ Just ("portInfo" .= _cippPortInfo),
            Just ("instanceName" .= _cippInstanceName)
          ]
      )

instance ToPath CloseInstancePublicPorts where
  toPath = const "/"

instance ToQuery CloseInstancePublicPorts where
  toQuery = const mempty

-- | /See:/ 'closeInstancePublicPortsResponse' smart constructor.
data CloseInstancePublicPortsResponse = CloseInstancePublicPortsResponse'
  { _cipprsOperation ::
      !(Maybe Operation),
    _cipprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloseInstancePublicPortsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cipprsOperation' - An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'cipprsResponseStatus' - -- | The response status code.
closeInstancePublicPortsResponse ::
  -- | 'cipprsResponseStatus'
  Int ->
  CloseInstancePublicPortsResponse
closeInstancePublicPortsResponse pResponseStatus_ =
  CloseInstancePublicPortsResponse'
    { _cipprsOperation = Nothing,
      _cipprsResponseStatus = pResponseStatus_
    }

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
cipprsOperation :: Lens' CloseInstancePublicPortsResponse (Maybe Operation)
cipprsOperation = lens _cipprsOperation (\s a -> s {_cipprsOperation = a})

-- | -- | The response status code.
cipprsResponseStatus :: Lens' CloseInstancePublicPortsResponse Int
cipprsResponseStatus = lens _cipprsResponseStatus (\s a -> s {_cipprsResponseStatus = a})

instance NFData CloseInstancePublicPortsResponse
