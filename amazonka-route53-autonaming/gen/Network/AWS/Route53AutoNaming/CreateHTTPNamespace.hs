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
-- Module      : Network.AWS.Route53AutoNaming.CreateHTTPNamespace
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HTTP namespace. Service instances that you register using an HTTP namespace can be discovered using a @DiscoverInstances@ request but can't be discovered using DNS.
--
--
-- For the current limit on the number of namespaces that you can create using the same AWS account, see <http://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits> in the /AWS Cloud Map Developer Guide/ .
--
module Network.AWS.Route53AutoNaming.CreateHTTPNamespace
    (
    -- * Creating a Request
      createHTTPNamespace
    , CreateHTTPNamespace
    -- * Request Lenses
    , chttpnCreatorRequestId
    , chttpnDescription
    , chttpnName

    -- * Destructuring the Response
    , createHTTPNamespaceResponse
    , CreateHTTPNamespaceResponse
    -- * Response Lenses
    , chttpnrsOperationId
    , chttpnrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'createHTTPNamespace' smart constructor.
data CreateHTTPNamespace = CreateHTTPNamespace'
  { _chttpnCreatorRequestId :: !(Maybe Text)
  , _chttpnDescription      :: !(Maybe Text)
  , _chttpnName             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHTTPNamespace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chttpnCreatorRequestId' - A unique string that identifies the request and that allows failed @CreateHttpNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- * 'chttpnDescription' - A description for the namespace.
--
-- * 'chttpnName' - The name that you want to assign to this namespace.
createHTTPNamespace
    :: Text -- ^ 'chttpnName'
    -> CreateHTTPNamespace
createHTTPNamespace pName_ =
  CreateHTTPNamespace'
    { _chttpnCreatorRequestId = Nothing
    , _chttpnDescription = Nothing
    , _chttpnName = pName_
    }


-- | A unique string that identifies the request and that allows failed @CreateHttpNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
chttpnCreatorRequestId :: Lens' CreateHTTPNamespace (Maybe Text)
chttpnCreatorRequestId = lens _chttpnCreatorRequestId (\ s a -> s{_chttpnCreatorRequestId = a})

-- | A description for the namespace.
chttpnDescription :: Lens' CreateHTTPNamespace (Maybe Text)
chttpnDescription = lens _chttpnDescription (\ s a -> s{_chttpnDescription = a})

-- | The name that you want to assign to this namespace.
chttpnName :: Lens' CreateHTTPNamespace Text
chttpnName = lens _chttpnName (\ s a -> s{_chttpnName = a})

instance AWSRequest CreateHTTPNamespace where
        type Rs CreateHTTPNamespace =
             CreateHTTPNamespaceResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 CreateHTTPNamespaceResponse' <$>
                   (x .?> "OperationId") <*> (pure (fromEnum s)))

instance Hashable CreateHTTPNamespace where

instance NFData CreateHTTPNamespace where

instance ToHeaders CreateHTTPNamespace where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.CreateHttpNamespace" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHTTPNamespace where
        toJSON CreateHTTPNamespace'{..}
          = object
              (catMaybes
                 [("CreatorRequestId" .=) <$> _chttpnCreatorRequestId,
                  ("Description" .=) <$> _chttpnDescription,
                  Just ("Name" .= _chttpnName)])

instance ToPath CreateHTTPNamespace where
        toPath = const "/"

instance ToQuery CreateHTTPNamespace where
        toQuery = const mempty

-- | /See:/ 'createHTTPNamespaceResponse' smart constructor.
data CreateHTTPNamespaceResponse = CreateHTTPNamespaceResponse'
  { _chttpnrsOperationId    :: !(Maybe Text)
  , _chttpnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHTTPNamespaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chttpnrsOperationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see 'GetOperation' .
--
-- * 'chttpnrsResponseStatus' - -- | The response status code.
createHTTPNamespaceResponse
    :: Int -- ^ 'chttpnrsResponseStatus'
    -> CreateHTTPNamespaceResponse
createHTTPNamespaceResponse pResponseStatus_ =
  CreateHTTPNamespaceResponse'
    {_chttpnrsOperationId = Nothing, _chttpnrsResponseStatus = pResponseStatus_}


-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see 'GetOperation' .
chttpnrsOperationId :: Lens' CreateHTTPNamespaceResponse (Maybe Text)
chttpnrsOperationId = lens _chttpnrsOperationId (\ s a -> s{_chttpnrsOperationId = a})

-- | -- | The response status code.
chttpnrsResponseStatus :: Lens' CreateHTTPNamespaceResponse Int
chttpnrsResponseStatus = lens _chttpnrsResponseStatus (\ s a -> s{_chttpnrsResponseStatus = a})

instance NFData CreateHTTPNamespaceResponse where
