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
-- Module      : Network.AWS.DirectoryService.ListCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For the specified directory, lists all the certificates registered for a secured LDAP connection.
module Network.AWS.DirectoryService.ListCertificates
  ( -- * Creating a Request
    listCertificates,
    ListCertificates,

    -- * Request Lenses
    lcNextToken,
    lcLimit,
    lcDirectoryId,

    -- * Destructuring the Response
    listCertificatesResponse,
    ListCertificatesResponse,

    -- * Response Lenses
    lcrsNextToken,
    lcrsCertificatesInfo,
    lcrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCertificates' smart constructor.
data ListCertificates = ListCertificates'
  { _lcNextToken ::
      !(Maybe Text),
    _lcLimit :: !(Maybe Nat),
    _lcDirectoryId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - A token for requesting another page of certificates if the @NextToken@ response element indicates that more certificates are available. Use the value of the returned @NextToken@ element in your request until the token comes back as @null@ . Pass @null@ if this is the first call.
--
-- * 'lcLimit' - The number of items that should show up on one page
--
-- * 'lcDirectoryId' - The identifier of the directory.
listCertificates ::
  -- | 'lcDirectoryId'
  Text ->
  ListCertificates
listCertificates pDirectoryId_ =
  ListCertificates'
    { _lcNextToken = Nothing,
      _lcLimit = Nothing,
      _lcDirectoryId = pDirectoryId_
    }

-- | A token for requesting another page of certificates if the @NextToken@ response element indicates that more certificates are available. Use the value of the returned @NextToken@ element in your request until the token comes back as @null@ . Pass @null@ if this is the first call.
lcNextToken :: Lens' ListCertificates (Maybe Text)
lcNextToken = lens _lcNextToken (\s a -> s {_lcNextToken = a})

-- | The number of items that should show up on one page
lcLimit :: Lens' ListCertificates (Maybe Natural)
lcLimit = lens _lcLimit (\s a -> s {_lcLimit = a}) . mapping _Nat

-- | The identifier of the directory.
lcDirectoryId :: Lens' ListCertificates Text
lcDirectoryId = lens _lcDirectoryId (\s a -> s {_lcDirectoryId = a})

instance AWSRequest ListCertificates where
  type Rs ListCertificates = ListCertificatesResponse
  request = postJSON directoryService
  response =
    receiveJSON
      ( \s h x ->
          ListCertificatesResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "CertificatesInfo" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListCertificates

instance NFData ListCertificates

instance ToHeaders ListCertificates where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.ListCertificates" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListCertificates where
  toJSON ListCertificates' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lcNextToken,
            ("Limit" .=) <$> _lcLimit,
            Just ("DirectoryId" .= _lcDirectoryId)
          ]
      )

instance ToPath ListCertificates where
  toPath = const "/"

instance ToQuery ListCertificates where
  toQuery = const mempty

-- | /See:/ 'listCertificatesResponse' smart constructor.
data ListCertificatesResponse = ListCertificatesResponse'
  { _lcrsNextToken ::
      !(Maybe Text),
    _lcrsCertificatesInfo ::
      !(Maybe [CertificateInfo]),
    _lcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsNextToken' - Indicates whether another page of certificates is available when the number of available certificates exceeds the page limit.
--
-- * 'lcrsCertificatesInfo' - A list of certificates with basic details including certificate ID, certificate common name, certificate state.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listCertificatesResponse ::
  -- | 'lcrsResponseStatus'
  Int ->
  ListCertificatesResponse
listCertificatesResponse pResponseStatus_ =
  ListCertificatesResponse'
    { _lcrsNextToken = Nothing,
      _lcrsCertificatesInfo = Nothing,
      _lcrsResponseStatus = pResponseStatus_
    }

-- | Indicates whether another page of certificates is available when the number of available certificates exceeds the page limit.
lcrsNextToken :: Lens' ListCertificatesResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\s a -> s {_lcrsNextToken = a})

-- | A list of certificates with basic details including certificate ID, certificate common name, certificate state.
lcrsCertificatesInfo :: Lens' ListCertificatesResponse [CertificateInfo]
lcrsCertificatesInfo = lens _lcrsCertificatesInfo (\s a -> s {_lcrsCertificatesInfo = a}) . _Default . _Coerce

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListCertificatesResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\s a -> s {_lcrsResponseStatus = a})

instance NFData ListCertificatesResponse
