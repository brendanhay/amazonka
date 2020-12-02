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
-- Module      : Network.AWS.Lightsail.UnpeerVPC
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to unpeer the Lightsail VPC from the user's default VPC.
--
--
module Network.AWS.Lightsail.UnpeerVPC
    (
    -- * Creating a Request
      unpeerVPC
    , UnpeerVPC

    -- * Destructuring the Response
    , unpeerVPCResponse
    , UnpeerVPCResponse
    -- * Response Lenses
    , uvrsOperation
    , uvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unpeerVPC' smart constructor.
data UnpeerVPC =
  UnpeerVPC'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnpeerVPC' with the minimum fields required to make a request.
--
unpeerVPC
    :: UnpeerVPC
unpeerVPC = UnpeerVPC'


instance AWSRequest UnpeerVPC where
        type Rs UnpeerVPC = UnpeerVPCResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 UnpeerVPCResponse' <$>
                   (x .?> "operation") <*> (pure (fromEnum s)))

instance Hashable UnpeerVPC where

instance NFData UnpeerVPC where

instance ToHeaders UnpeerVPC where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.UnpeerVpc" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UnpeerVPC where
        toJSON = const (Object mempty)

instance ToPath UnpeerVPC where
        toPath = const "/"

instance ToQuery UnpeerVPC where
        toQuery = const mempty

-- | /See:/ 'unpeerVPCResponse' smart constructor.
data UnpeerVPCResponse = UnpeerVPCResponse'
  { _uvrsOperation      :: !(Maybe Operation)
  , _uvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnpeerVPCResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvrsOperation' - An array of key-value pairs containing information about the request operation.
--
-- * 'uvrsResponseStatus' - -- | The response status code.
unpeerVPCResponse
    :: Int -- ^ 'uvrsResponseStatus'
    -> UnpeerVPCResponse
unpeerVPCResponse pResponseStatus_ =
  UnpeerVPCResponse'
    {_uvrsOperation = Nothing, _uvrsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the request operation.
uvrsOperation :: Lens' UnpeerVPCResponse (Maybe Operation)
uvrsOperation = lens _uvrsOperation (\ s a -> s{_uvrsOperation = a})

-- | -- | The response status code.
uvrsResponseStatus :: Lens' UnpeerVPCResponse Int
uvrsResponseStatus = lens _uvrsResponseStatus (\ s a -> s{_uvrsResponseStatus = a})

instance NFData UnpeerVPCResponse where
