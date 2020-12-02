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
-- Module      : Network.AWS.CertificateManagerPCA.ListTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags, if any, that are associated with your private CA. Tags are labels that you can use to identify and organize your CAs. Each tag consists of a key and an optional value. Call the 'TagCertificateAuthority' function to add one or more tags to your CA. Call the 'UntagCertificateAuthority' function to remove tags.
--
--
module Network.AWS.CertificateManagerPCA.ListTags
    (
    -- * Creating a Request
      listTags
    , ListTags
    -- * Request Lenses
    , ltNextToken
    , ltMaxResults
    , ltCertificateAuthorityARN

    -- * Destructuring the Response
    , listTagsResponse
    , ListTagsResponse
    -- * Response Lenses
    , ltrsNextToken
    , ltrsTags
    , ltrsResponseStatus
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTags' smart constructor.
data ListTags = ListTags'
  { _ltNextToken               :: !(Maybe Text)
  , _ltMaxResults              :: !(Maybe Nat)
  , _ltCertificateAuthorityARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltNextToken' - Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
--
-- * 'ltMaxResults' - Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
--
-- * 'ltCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called the 'CreateCertificateAuthority' function. This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
listTags
    :: Text -- ^ 'ltCertificateAuthorityARN'
    -> ListTags
listTags pCertificateAuthorityARN_ =
  ListTags'
    { _ltNextToken = Nothing
    , _ltMaxResults = Nothing
    , _ltCertificateAuthorityARN = pCertificateAuthorityARN_
    }


-- | Use this parameter when paginating results in a subsequent request after you receive a response with truncated results. Set it to the value of __NextToken__ from the response you just received.
ltNextToken :: Lens' ListTags (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a})

-- | Use this parameter when paginating results to specify the maximum number of items to return in the response. If additional items exist beyond the number you specify, the __NextToken__ element is sent in the response. Use this __NextToken__ value in a subsequent request to retrieve additional items.
ltMaxResults :: Lens' ListTags (Maybe Natural)
ltMaxResults = lens _ltMaxResults (\ s a -> s{_ltMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that was returned when you called the 'CreateCertificateAuthority' function. This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
ltCertificateAuthorityARN :: Lens' ListTags Text
ltCertificateAuthorityARN = lens _ltCertificateAuthorityARN (\ s a -> s{_ltCertificateAuthorityARN = a})

instance AWSRequest ListTags where
        type Rs ListTags = ListTagsResponse
        request = postJSON certificateManagerPCA
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Tags") <*>
                     (pure (fromEnum s)))

instance Hashable ListTags where

instance NFData ListTags where

instance ToHeaders ListTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.ListTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTags where
        toJSON ListTags'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltNextToken,
                  ("MaxResults" .=) <$> _ltMaxResults,
                  Just
                    ("CertificateAuthorityArn" .=
                       _ltCertificateAuthorityARN)])

instance ToPath ListTags where
        toPath = const "/"

instance ToQuery ListTags where
        toQuery = const mempty

-- | /See:/ 'listTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { _ltrsNextToken      :: !(Maybe Text)
  , _ltrsTags           :: !(Maybe (List1 Tag))
  , _ltrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrsNextToken' - When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request.
--
-- * 'ltrsTags' - The tags associated with your private CA.
--
-- * 'ltrsResponseStatus' - -- | The response status code.
listTagsResponse
    :: Int -- ^ 'ltrsResponseStatus'
    -> ListTagsResponse
listTagsResponse pResponseStatus_ =
  ListTagsResponse'
    { _ltrsNextToken = Nothing
    , _ltrsTags = Nothing
    , _ltrsResponseStatus = pResponseStatus_
    }


-- | When the list is truncated, this value is present and should be used for the __NextToken__ parameter in a subsequent pagination request.
ltrsNextToken :: Lens' ListTagsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a})

-- | The tags associated with your private CA.
ltrsTags :: Lens' ListTagsResponse (Maybe (NonEmpty Tag))
ltrsTags = lens _ltrsTags (\ s a -> s{_ltrsTags = a}) . mapping _List1

-- | -- | The response status code.
ltrsResponseStatus :: Lens' ListTagsResponse Int
ltrsResponseStatus = lens _ltrsResponseStatus (\ s a -> s{_ltrsResponseStatus = a})

instance NFData ListTagsResponse where
