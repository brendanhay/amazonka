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
-- Module      : Network.AWS.Lightsail.CreateDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain resource for the specified domain (e.g., example.com).
--
--
module Network.AWS.Lightsail.CreateDomain
    (
    -- * Creating a Request
      createDomain
    , CreateDomain
    -- * Request Lenses
    , cdDomainName

    -- * Destructuring the Response
    , createDomainResponse
    , CreateDomainResponse
    -- * Response Lenses
    , cdrsOperation
    , cdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDomain' smart constructor.
newtype CreateDomain = CreateDomain'
  { _cdDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdDomainName' - The domain name to manage (e.g., @example.com@ ).
createDomain
    :: Text -- ^ 'cdDomainName'
    -> CreateDomain
createDomain pDomainName_ = CreateDomain' {_cdDomainName = pDomainName_}


-- | The domain name to manage (e.g., @example.com@ ).
cdDomainName :: Lens' CreateDomain Text
cdDomainName = lens _cdDomainName (\ s a -> s{_cdDomainName = a})

instance AWSRequest CreateDomain where
        type Rs CreateDomain = CreateDomainResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CreateDomainResponse' <$>
                   (x .?> "operation") <*> (pure (fromEnum s)))

instance Hashable CreateDomain where

instance NFData CreateDomain where

instance ToHeaders CreateDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CreateDomain" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDomain where
        toJSON CreateDomain'{..}
          = object
              (catMaybes [Just ("domainName" .= _cdDomainName)])

instance ToPath CreateDomain where
        toPath = const "/"

instance ToQuery CreateDomain where
        toQuery = const mempty

-- | /See:/ 'createDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { _cdrsOperation      :: !(Maybe Operation)
  , _cdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsOperation' - An array of key-value pairs containing information about the domain resource you created.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
createDomainResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> CreateDomainResponse
createDomainResponse pResponseStatus_ =
  CreateDomainResponse'
    {_cdrsOperation = Nothing, _cdrsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the domain resource you created.
cdrsOperation :: Lens' CreateDomainResponse (Maybe Operation)
cdrsOperation = lens _cdrsOperation (\ s a -> s{_cdrsOperation = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDomainResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData CreateDomainResponse where
