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
-- Module      : Network.AWS.SecretsManager.ListSecrets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the secrets that are stored by Secrets Manager in the AWS account. To list the versions currently stored for a specific secret, use 'ListSecretVersionIds' . The encrypted fields @SecretString@ and @SecretBinary@ are not included in the output. To get that information, call the 'GetSecretValue' operation.
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:ListSecrets
--
--
--
-- __Related operations__
--
--     * To list the versions attached to a secret, use 'ListSecretVersionIds' .
--
--
--
module Network.AWS.SecretsManager.ListSecrets
    (
    -- * Creating a Request
      listSecrets
    , ListSecrets
    -- * Request Lenses
    , lsNextToken
    , lsMaxResults

    -- * Destructuring the Response
    , listSecretsResponse
    , ListSecretsResponse
    -- * Response Lenses
    , lsrsNextToken
    , lsrsSecretList
    , lsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'listSecrets' smart constructor.
data ListSecrets = ListSecrets'
  { _lsNextToken  :: !(Maybe Text)
  , _lsMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSecrets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsNextToken' - (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request that indicates that there's more output available. In a subsequent call, set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lsMaxResults' - (Optional) Limits the number of results that you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
listSecrets
    :: ListSecrets
listSecrets = ListSecrets' {_lsNextToken = Nothing, _lsMaxResults = Nothing}


-- | (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request that indicates that there's more output available. In a subsequent call, set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lsNextToken :: Lens' ListSecrets (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a})

-- | (Optional) Limits the number of results that you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lsMaxResults :: Lens' ListSecrets (Maybe Natural)
lsMaxResults = lens _lsMaxResults (\ s a -> s{_lsMaxResults = a}) . mapping _Nat

instance AWSRequest ListSecrets where
        type Rs ListSecrets = ListSecretsResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 ListSecretsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "SecretList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListSecrets where

instance NFData ListSecrets where

instance ToHeaders ListSecrets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.ListSecrets" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSecrets where
        toJSON ListSecrets'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lsNextToken,
                  ("MaxResults" .=) <$> _lsMaxResults])

instance ToPath ListSecrets where
        toPath = const "/"

instance ToQuery ListSecrets where
        toQuery = const mempty

-- | /See:/ 'listSecretsResponse' smart constructor.
data ListSecretsResponse = ListSecretsResponse'
  { _lsrsNextToken      :: !(Maybe Text)
  , _lsrsSecretList     :: !(Maybe [SecretListEntry])
  , _lsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSecretsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrsNextToken' - If present in the response, this value indicates that there's more output available than what's included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
--
-- * 'lsrsSecretList' - A list of the secrets in the account.
--
-- * 'lsrsResponseStatus' - -- | The response status code.
listSecretsResponse
    :: Int -- ^ 'lsrsResponseStatus'
    -> ListSecretsResponse
listSecretsResponse pResponseStatus_ =
  ListSecretsResponse'
    { _lsrsNextToken = Nothing
    , _lsrsSecretList = Nothing
    , _lsrsResponseStatus = pResponseStatus_
    }


-- | If present in the response, this value indicates that there's more output available than what's included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
lsrsNextToken :: Lens' ListSecretsResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a})

-- | A list of the secrets in the account.
lsrsSecretList :: Lens' ListSecretsResponse [SecretListEntry]
lsrsSecretList = lens _lsrsSecretList (\ s a -> s{_lsrsSecretList = a}) . _Default . _Coerce

-- | -- | The response status code.
lsrsResponseStatus :: Lens' ListSecretsResponse Int
lsrsResponseStatus = lens _lsrsResponseStatus (\ s a -> s{_lsrsResponseStatus = a})

instance NFData ListSecretsResponse where
