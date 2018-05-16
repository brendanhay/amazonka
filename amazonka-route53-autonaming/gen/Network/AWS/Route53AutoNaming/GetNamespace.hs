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
-- Module      : Network.AWS.Route53AutoNaming.GetNamespace
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a namespace.
--
--
module Network.AWS.Route53AutoNaming.GetNamespace
    (
    -- * Creating a Request
      getNamespace
    , GetNamespace
    -- * Request Lenses
    , gnId

    -- * Destructuring the Response
    , getNamespaceResponse
    , GetNamespaceResponse
    -- * Response Lenses
    , gnrsNamespace
    , gnrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'getNamespace' smart constructor.
newtype GetNamespace = GetNamespace'
  { _gnId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetNamespace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gnId' - The ID of the namespace that you want to get information about.
getNamespace
    :: Text -- ^ 'gnId'
    -> GetNamespace
getNamespace pId_ = GetNamespace' {_gnId = pId_}


-- | The ID of the namespace that you want to get information about.
gnId :: Lens' GetNamespace Text
gnId = lens _gnId (\ s a -> s{_gnId = a})

instance AWSRequest GetNamespace where
        type Rs GetNamespace = GetNamespaceResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 GetNamespaceResponse' <$>
                   (x .?> "Namespace") <*> (pure (fromEnum s)))

instance Hashable GetNamespace where

instance NFData GetNamespace where

instance ToHeaders GetNamespace where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.GetNamespace" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetNamespace where
        toJSON GetNamespace'{..}
          = object (catMaybes [Just ("Id" .= _gnId)])

instance ToPath GetNamespace where
        toPath = const "/"

instance ToQuery GetNamespace where
        toQuery = const mempty

-- | /See:/ 'getNamespaceResponse' smart constructor.
data GetNamespaceResponse = GetNamespaceResponse'
  { _gnrsNamespace      :: !(Maybe Namespace)
  , _gnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetNamespaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gnrsNamespace' - A complex type that contains information about the specified namespace.
--
-- * 'gnrsResponseStatus' - -- | The response status code.
getNamespaceResponse
    :: Int -- ^ 'gnrsResponseStatus'
    -> GetNamespaceResponse
getNamespaceResponse pResponseStatus_ =
  GetNamespaceResponse'
    {_gnrsNamespace = Nothing, _gnrsResponseStatus = pResponseStatus_}


-- | A complex type that contains information about the specified namespace.
gnrsNamespace :: Lens' GetNamespaceResponse (Maybe Namespace)
gnrsNamespace = lens _gnrsNamespace (\ s a -> s{_gnrsNamespace = a})

-- | -- | The response status code.
gnrsResponseStatus :: Lens' GetNamespaceResponse Int
gnrsResponseStatus = lens _gnrsResponseStatus (\ s a -> s{_gnrsResponseStatus = a})

instance NFData GetNamespaceResponse where
