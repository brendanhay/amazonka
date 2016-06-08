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
-- Module      : Network.AWS.APIGateway.UpdateAccount
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the current < Account> resource.
module Network.AWS.APIGateway.UpdateAccount
    (
    -- * Creating a Request
      updateAccount
    , UpdateAccount
    -- * Request Lenses
    , uaPatchOperations

    -- * Destructuring the Response
    , account
    , Account
    -- * Response Lenses
    , aCloudwatchRoleARN
    , aThrottleSettings
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Requests Amazon API Gateway to change information about the current < Account> resource.
--
-- /See:/ 'updateAccount' smart constructor.
newtype UpdateAccount = UpdateAccount'
    { _uaPatchOperations :: Maybe [PatchOperation]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaPatchOperations'
updateAccount
    :: UpdateAccount
updateAccount =
    UpdateAccount'
    { _uaPatchOperations = Nothing
    }

-- | A list of operations describing the updates to apply to the specified resource. The patches are applied in the order specified in the list.
uaPatchOperations :: Lens' UpdateAccount [PatchOperation]
uaPatchOperations = lens _uaPatchOperations (\ s a -> s{_uaPatchOperations = a}) . _Default . _Coerce;

instance AWSRequest UpdateAccount where
        type Rs UpdateAccount = Account
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateAccount

instance NFData UpdateAccount

instance ToHeaders UpdateAccount where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateAccount where
        toJSON UpdateAccount'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uaPatchOperations])

instance ToPath UpdateAccount where
        toPath = const "/account"

instance ToQuery UpdateAccount where
        toQuery = const mempty
