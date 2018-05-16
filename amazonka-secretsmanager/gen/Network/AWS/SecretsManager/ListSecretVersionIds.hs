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
-- Module      : Network.AWS.SecretsManager.ListSecretVersionIds
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the versions attached to the specified secret. The output does not include the @SecretString@ or @SecretBinary@ fields. By default, the list includes only versions that have at least one staging label in @VersionStage@ attached.
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:ListSecretVersionIds
--
--
--
-- __Related operations__
--
--     * To list the secrets in an account, use 'ListSecrets' .
--
--
--
module Network.AWS.SecretsManager.ListSecretVersionIds
    (
    -- * Creating a Request
      listSecretVersionIds
    , ListSecretVersionIds
    -- * Request Lenses
    , lsviNextToken
    , lsviIncludeDeprecated
    , lsviMaxResults
    , lsviSecretId

    -- * Destructuring the Response
    , listSecretVersionIdsResponse
    , ListSecretVersionIdsResponse
    -- * Response Lenses
    , lsvirsARN
    , lsvirsVersions
    , lsvirsNextToken
    , lsvirsName
    , lsvirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'listSecretVersionIds' smart constructor.
data ListSecretVersionIds = ListSecretVersionIds'
  { _lsviNextToken         :: !(Maybe Text)
  , _lsviIncludeDeprecated :: !(Maybe Bool)
  , _lsviMaxResults        :: !(Maybe Nat)
  , _lsviSecretId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSecretVersionIds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsviNextToken' - (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request that indicates that there's more output available. In a subsequent call, set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lsviIncludeDeprecated' - (Optional) Specifies that you want the results to include versions that do not have any staging labels attached to them. Such versions are considered deprecated and are subject to deletion by Secrets Manager as needed.
--
-- * 'lsviMaxResults' - (Optional) Limits the number of results that you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- * 'lsviSecretId' - The identifier for the secret containing the versions you want to list. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
listSecretVersionIds
    :: Text -- ^ 'lsviSecretId'
    -> ListSecretVersionIds
listSecretVersionIds pSecretId_ =
  ListSecretVersionIds'
    { _lsviNextToken = Nothing
    , _lsviIncludeDeprecated = Nothing
    , _lsviMaxResults = Nothing
    , _lsviSecretId = pSecretId_
    }


-- | (Optional) Use this parameter in a request if you receive a @NextToken@ response in a previous request that indicates that there's more output available. In a subsequent call, set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
lsviNextToken :: Lens' ListSecretVersionIds (Maybe Text)
lsviNextToken = lens _lsviNextToken (\ s a -> s{_lsviNextToken = a})

-- | (Optional) Specifies that you want the results to include versions that do not have any staging labels attached to them. Such versions are considered deprecated and are subject to deletion by Secrets Manager as needed.
lsviIncludeDeprecated :: Lens' ListSecretVersionIds (Maybe Bool)
lsviIncludeDeprecated = lens _lsviIncludeDeprecated (\ s a -> s{_lsviIncludeDeprecated = a})

-- | (Optional) Limits the number of results that you want to include in the response. If you don't include this parameter, it defaults to a value that's specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (isn't null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Secrets Manager might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lsviMaxResults :: Lens' ListSecretVersionIds (Maybe Natural)
lsviMaxResults = lens _lsviMaxResults (\ s a -> s{_lsviMaxResults = a}) . mapping _Nat

-- | The identifier for the secret containing the versions you want to list. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
lsviSecretId :: Lens' ListSecretVersionIds Text
lsviSecretId = lens _lsviSecretId (\ s a -> s{_lsviSecretId = a})

instance AWSRequest ListSecretVersionIds where
        type Rs ListSecretVersionIds =
             ListSecretVersionIdsResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 ListSecretVersionIdsResponse' <$>
                   (x .?> "ARN") <*> (x .?> "Versions" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (x .?> "Name")
                     <*> (pure (fromEnum s)))

instance Hashable ListSecretVersionIds where

instance NFData ListSecretVersionIds where

instance ToHeaders ListSecretVersionIds where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.ListSecretVersionIds" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSecretVersionIds where
        toJSON ListSecretVersionIds'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lsviNextToken,
                  ("IncludeDeprecated" .=) <$> _lsviIncludeDeprecated,
                  ("MaxResults" .=) <$> _lsviMaxResults,
                  Just ("SecretId" .= _lsviSecretId)])

instance ToPath ListSecretVersionIds where
        toPath = const "/"

instance ToQuery ListSecretVersionIds where
        toQuery = const mempty

-- | /See:/ 'listSecretVersionIdsResponse' smart constructor.
data ListSecretVersionIdsResponse = ListSecretVersionIdsResponse'
  { _lsvirsARN            :: !(Maybe Text)
  , _lsvirsVersions       :: !(Maybe [SecretVersionsListEntry])
  , _lsvirsNextToken      :: !(Maybe Text)
  , _lsvirsName           :: !(Maybe Text)
  , _lsvirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSecretVersionIdsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsvirsARN' - The Amazon Resource Name (ARN) for the secret.
--
-- * 'lsvirsVersions' - The list of the currently available versions of the specified secret.
--
-- * 'lsvirsNextToken' - If present in the response, this value indicates that there's more output available than what's included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
--
-- * 'lsvirsName' - The friendly name of the secret.
--
-- * 'lsvirsResponseStatus' - -- | The response status code.
listSecretVersionIdsResponse
    :: Int -- ^ 'lsvirsResponseStatus'
    -> ListSecretVersionIdsResponse
listSecretVersionIdsResponse pResponseStatus_ =
  ListSecretVersionIdsResponse'
    { _lsvirsARN = Nothing
    , _lsvirsVersions = Nothing
    , _lsvirsNextToken = Nothing
    , _lsvirsName = Nothing
    , _lsvirsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) for the secret.
lsvirsARN :: Lens' ListSecretVersionIdsResponse (Maybe Text)
lsvirsARN = lens _lsvirsARN (\ s a -> s{_lsvirsARN = a})

-- | The list of the currently available versions of the specified secret.
lsvirsVersions :: Lens' ListSecretVersionIdsResponse [SecretVersionsListEntry]
lsvirsVersions = lens _lsvirsVersions (\ s a -> s{_lsvirsVersions = a}) . _Default . _Coerce

-- | If present in the response, this value indicates that there's more output available than what's included in the current response. This can occur even when the response includes no values at all, such as when you ask for a filtered view of a very long list. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to continue processing and get the next part of the output. You should repeat this until the @NextToken@ response element comes back empty (as @null@ ).
lsvirsNextToken :: Lens' ListSecretVersionIdsResponse (Maybe Text)
lsvirsNextToken = lens _lsvirsNextToken (\ s a -> s{_lsvirsNextToken = a})

-- | The friendly name of the secret.
lsvirsName :: Lens' ListSecretVersionIdsResponse (Maybe Text)
lsvirsName = lens _lsvirsName (\ s a -> s{_lsvirsName = a})

-- | -- | The response status code.
lsvirsResponseStatus :: Lens' ListSecretVersionIdsResponse Int
lsvirsResponseStatus = lens _lsvirsResponseStatus (\ s a -> s{_lsvirsResponseStatus = a})

instance NFData ListSecretVersionIdsResponse where
