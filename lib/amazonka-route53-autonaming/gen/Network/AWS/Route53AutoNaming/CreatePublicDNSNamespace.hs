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
-- Module      : Network.AWS.Route53AutoNaming.CreatePublicDNSNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a public namespace based on DNS, which will be visible on the internet. The namespace defines your service naming scheme. For example, if you name your namespace @example.com@ and name your service @backend@ , the resulting DNS name for the service will be @backend.example.com@ . For the current quota on the number of namespaces that you can create using the same AWS account, see <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits> in the /AWS Cloud Map Developer Guide/ .
module Network.AWS.Route53AutoNaming.CreatePublicDNSNamespace
  ( -- * Creating a Request
    createPublicDNSNamespace,
    CreatePublicDNSNamespace,

    -- * Request Lenses
    cpdnCreatorRequestId,
    cpdnDescription,
    cpdnTags,
    cpdnName,

    -- * Destructuring the Response
    createPublicDNSNamespaceResponse,
    CreatePublicDNSNamespaceResponse,

    -- * Response Lenses
    cpdnrsOperationId,
    cpdnrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'createPublicDNSNamespace' smart constructor.
data CreatePublicDNSNamespace = CreatePublicDNSNamespace'
  { _cpdnCreatorRequestId ::
      !(Maybe Text),
    _cpdnDescription :: !(Maybe Text),
    _cpdnTags :: !(Maybe [Tag]),
    _cpdnName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePublicDNSNamespace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpdnCreatorRequestId' - A unique string that identifies the request and that allows failed @CreatePublicDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- * 'cpdnDescription' - A description for the namespace.
--
-- * 'cpdnTags' - The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- * 'cpdnName' - The name that you want to assign to this namespace.
createPublicDNSNamespace ::
  -- | 'cpdnName'
  Text ->
  CreatePublicDNSNamespace
createPublicDNSNamespace pName_ =
  CreatePublicDNSNamespace'
    { _cpdnCreatorRequestId = Nothing,
      _cpdnDescription = Nothing,
      _cpdnTags = Nothing,
      _cpdnName = pName_
    }

-- | A unique string that identifies the request and that allows failed @CreatePublicDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
cpdnCreatorRequestId :: Lens' CreatePublicDNSNamespace (Maybe Text)
cpdnCreatorRequestId = lens _cpdnCreatorRequestId (\s a -> s {_cpdnCreatorRequestId = a})

-- | A description for the namespace.
cpdnDescription :: Lens' CreatePublicDNSNamespace (Maybe Text)
cpdnDescription = lens _cpdnDescription (\s a -> s {_cpdnDescription = a})

-- | The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
cpdnTags :: Lens' CreatePublicDNSNamespace [Tag]
cpdnTags = lens _cpdnTags (\s a -> s {_cpdnTags = a}) . _Default . _Coerce

-- | The name that you want to assign to this namespace.
cpdnName :: Lens' CreatePublicDNSNamespace Text
cpdnName = lens _cpdnName (\s a -> s {_cpdnName = a})

instance AWSRequest CreatePublicDNSNamespace where
  type Rs CreatePublicDNSNamespace = CreatePublicDNSNamespaceResponse
  request = postJSON route53AutoNaming
  response =
    receiveJSON
      ( \s h x ->
          CreatePublicDNSNamespaceResponse'
            <$> (x .?> "OperationId") <*> (pure (fromEnum s))
      )

instance Hashable CreatePublicDNSNamespace

instance NFData CreatePublicDNSNamespace

instance ToHeaders CreatePublicDNSNamespace where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Route53AutoNaming_v20170314.CreatePublicDnsNamespace" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreatePublicDNSNamespace where
  toJSON CreatePublicDNSNamespace' {..} =
    object
      ( catMaybes
          [ ("CreatorRequestId" .=) <$> _cpdnCreatorRequestId,
            ("Description" .=) <$> _cpdnDescription,
            ("Tags" .=) <$> _cpdnTags,
            Just ("Name" .= _cpdnName)
          ]
      )

instance ToPath CreatePublicDNSNamespace where
  toPath = const "/"

instance ToQuery CreatePublicDNSNamespace where
  toQuery = const mempty

-- | /See:/ 'createPublicDNSNamespaceResponse' smart constructor.
data CreatePublicDNSNamespaceResponse = CreatePublicDNSNamespaceResponse'
  { _cpdnrsOperationId ::
      !(Maybe Text),
    _cpdnrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreatePublicDNSNamespaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpdnrsOperationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- * 'cpdnrsResponseStatus' - -- | The response status code.
createPublicDNSNamespaceResponse ::
  -- | 'cpdnrsResponseStatus'
  Int ->
  CreatePublicDNSNamespaceResponse
createPublicDNSNamespaceResponse pResponseStatus_ =
  CreatePublicDNSNamespaceResponse'
    { _cpdnrsOperationId = Nothing,
      _cpdnrsResponseStatus = pResponseStatus_
    }

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
cpdnrsOperationId :: Lens' CreatePublicDNSNamespaceResponse (Maybe Text)
cpdnrsOperationId = lens _cpdnrsOperationId (\s a -> s {_cpdnrsOperationId = a})

-- | -- | The response status code.
cpdnrsResponseStatus :: Lens' CreatePublicDNSNamespaceResponse Int
cpdnrsResponseStatus = lens _cpdnrsResponseStatus (\s a -> s {_cpdnrsResponseStatus = a})

instance NFData CreatePublicDNSNamespaceResponse
