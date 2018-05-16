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
-- Module      : Network.AWS.Lightsail.IsVPCPeered
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a Boolean value indicating whether your Lightsail VPC is peered.
--
--
module Network.AWS.Lightsail.IsVPCPeered
    (
    -- * Creating a Request
      isVPCPeered
    , IsVPCPeered

    -- * Destructuring the Response
    , isVPCPeeredResponse
    , IsVPCPeeredResponse
    -- * Response Lenses
    , ivprsIsPeered
    , ivprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'isVPCPeered' smart constructor.
data IsVPCPeered =
  IsVPCPeered'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IsVPCPeered' with the minimum fields required to make a request.
--
isVPCPeered
    :: IsVPCPeered
isVPCPeered = IsVPCPeered'


instance AWSRequest IsVPCPeered where
        type Rs IsVPCPeered = IsVPCPeeredResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 IsVPCPeeredResponse' <$>
                   (x .?> "isPeered") <*> (pure (fromEnum s)))

instance Hashable IsVPCPeered where

instance NFData IsVPCPeered where

instance ToHeaders IsVPCPeered where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.IsVpcPeered" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON IsVPCPeered where
        toJSON = const (Object mempty)

instance ToPath IsVPCPeered where
        toPath = const "/"

instance ToQuery IsVPCPeered where
        toQuery = const mempty

-- | /See:/ 'isVPCPeeredResponse' smart constructor.
data IsVPCPeeredResponse = IsVPCPeeredResponse'
  { _ivprsIsPeered       :: !(Maybe Bool)
  , _ivprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IsVPCPeeredResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivprsIsPeered' - Returns @true@ if the Lightsail VPC is peered; otherwise, @false@ .
--
-- * 'ivprsResponseStatus' - -- | The response status code.
isVPCPeeredResponse
    :: Int -- ^ 'ivprsResponseStatus'
    -> IsVPCPeeredResponse
isVPCPeeredResponse pResponseStatus_ =
  IsVPCPeeredResponse'
    {_ivprsIsPeered = Nothing, _ivprsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the Lightsail VPC is peered; otherwise, @false@ .
ivprsIsPeered :: Lens' IsVPCPeeredResponse (Maybe Bool)
ivprsIsPeered = lens _ivprsIsPeered (\ s a -> s{_ivprsIsPeered = a})

-- | -- | The response status code.
ivprsResponseStatus :: Lens' IsVPCPeeredResponse Int
ivprsResponseStatus = lens _ivprsResponseStatus (\ s a -> s{_ivprsResponseStatus = a})

instance NFData IsVPCPeeredResponse where
