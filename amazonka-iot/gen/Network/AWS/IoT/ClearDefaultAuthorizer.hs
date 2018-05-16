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
-- Module      : Network.AWS.IoT.ClearDefaultAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears the default authorizer.
--
--
module Network.AWS.IoT.ClearDefaultAuthorizer
    (
    -- * Creating a Request
      clearDefaultAuthorizer
    , ClearDefaultAuthorizer

    -- * Destructuring the Response
    , clearDefaultAuthorizerResponse
    , ClearDefaultAuthorizerResponse
    -- * Response Lenses
    , cdarsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'clearDefaultAuthorizer' smart constructor.
data ClearDefaultAuthorizer =
  ClearDefaultAuthorizer'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClearDefaultAuthorizer' with the minimum fields required to make a request.
--
clearDefaultAuthorizer
    :: ClearDefaultAuthorizer
clearDefaultAuthorizer = ClearDefaultAuthorizer'


instance AWSRequest ClearDefaultAuthorizer where
        type Rs ClearDefaultAuthorizer =
             ClearDefaultAuthorizerResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 ClearDefaultAuthorizerResponse' <$>
                   (pure (fromEnum s)))

instance Hashable ClearDefaultAuthorizer where

instance NFData ClearDefaultAuthorizer where

instance ToHeaders ClearDefaultAuthorizer where
        toHeaders = const mempty

instance ToPath ClearDefaultAuthorizer where
        toPath = const "/default-authorizer"

instance ToQuery ClearDefaultAuthorizer where
        toQuery = const mempty

-- | /See:/ 'clearDefaultAuthorizerResponse' smart constructor.
newtype ClearDefaultAuthorizerResponse = ClearDefaultAuthorizerResponse'
  { _cdarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ClearDefaultAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdarsResponseStatus' - -- | The response status code.
clearDefaultAuthorizerResponse
    :: Int -- ^ 'cdarsResponseStatus'
    -> ClearDefaultAuthorizerResponse
clearDefaultAuthorizerResponse pResponseStatus_ =
  ClearDefaultAuthorizerResponse' {_cdarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cdarsResponseStatus :: Lens' ClearDefaultAuthorizerResponse Int
cdarsResponseStatus = lens _cdarsResponseStatus (\ s a -> s{_cdarsResponseStatus = a})

instance NFData ClearDefaultAuthorizerResponse where
