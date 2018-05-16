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
-- Module      : Network.AWS.Athena.CreateNamedQuery
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a named query.
--
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
--
module Network.AWS.Athena.CreateNamedQuery
    (
    -- * Creating a Request
      createNamedQuery
    , CreateNamedQuery
    -- * Request Lenses
    , cnqClientRequestToken
    , cnqDescription
    , cnqName
    , cnqDatabase
    , cnqQueryString

    -- * Destructuring the Response
    , createNamedQueryResponse
    , CreateNamedQueryResponse
    -- * Response Lenses
    , cnqrsNamedQueryId
    , cnqrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createNamedQuery' smart constructor.
data CreateNamedQuery = CreateNamedQuery'
  { _cnqClientRequestToken :: !(Maybe Text)
  , _cnqDescription        :: !(Maybe Text)
  , _cnqName               :: !Text
  , _cnqDatabase           :: !Text
  , _cnqQueryString        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNamedQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnqClientRequestToken' - A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @CreateNamedQuery@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned. /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
--
-- * 'cnqDescription' - A brief explanation of the query.
--
-- * 'cnqName' - The plain language name for the query.
--
-- * 'cnqDatabase' - The database to which the query belongs.
--
-- * 'cnqQueryString' - The text of the query itself. In other words, all query statements.
createNamedQuery
    :: Text -- ^ 'cnqName'
    -> Text -- ^ 'cnqDatabase'
    -> Text -- ^ 'cnqQueryString'
    -> CreateNamedQuery
createNamedQuery pName_ pDatabase_ pQueryString_ =
  CreateNamedQuery'
    { _cnqClientRequestToken = Nothing
    , _cnqDescription = Nothing
    , _cnqName = pName_
    , _cnqDatabase = pDatabase_
    , _cnqQueryString = pQueryString_
    }


-- | A unique case-sensitive string used to ensure the request to create the query is idempotent (executes only once). If another @CreateNamedQuery@ request is received, the same response is returned and another query is not created. If a parameter has changed, for example, the @QueryString@ , an error is returned. /Important:/ This token is listed as not required because AWS SDKs (for example the AWS SDK for Java) auto-generate the token for users. If you are not using the AWS SDK or the AWS CLI, you must provide this token or the action will fail.
cnqClientRequestToken :: Lens' CreateNamedQuery (Maybe Text)
cnqClientRequestToken = lens _cnqClientRequestToken (\ s a -> s{_cnqClientRequestToken = a})

-- | A brief explanation of the query.
cnqDescription :: Lens' CreateNamedQuery (Maybe Text)
cnqDescription = lens _cnqDescription (\ s a -> s{_cnqDescription = a})

-- | The plain language name for the query.
cnqName :: Lens' CreateNamedQuery Text
cnqName = lens _cnqName (\ s a -> s{_cnqName = a})

-- | The database to which the query belongs.
cnqDatabase :: Lens' CreateNamedQuery Text
cnqDatabase = lens _cnqDatabase (\ s a -> s{_cnqDatabase = a})

-- | The text of the query itself. In other words, all query statements.
cnqQueryString :: Lens' CreateNamedQuery Text
cnqQueryString = lens _cnqQueryString (\ s a -> s{_cnqQueryString = a})

instance AWSRequest CreateNamedQuery where
        type Rs CreateNamedQuery = CreateNamedQueryResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 CreateNamedQueryResponse' <$>
                   (x .?> "NamedQueryId") <*> (pure (fromEnum s)))

instance Hashable CreateNamedQuery where

instance NFData CreateNamedQuery where

instance ToHeaders CreateNamedQuery where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.CreateNamedQuery" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateNamedQuery where
        toJSON CreateNamedQuery'{..}
          = object
              (catMaybes
                 [("ClientRequestToken" .=) <$>
                    _cnqClientRequestToken,
                  ("Description" .=) <$> _cnqDescription,
                  Just ("Name" .= _cnqName),
                  Just ("Database" .= _cnqDatabase),
                  Just ("QueryString" .= _cnqQueryString)])

instance ToPath CreateNamedQuery where
        toPath = const "/"

instance ToQuery CreateNamedQuery where
        toQuery = const mempty

-- | /See:/ 'createNamedQueryResponse' smart constructor.
data CreateNamedQueryResponse = CreateNamedQueryResponse'
  { _cnqrsNamedQueryId   :: !(Maybe Text)
  , _cnqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNamedQueryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnqrsNamedQueryId' - The unique ID of the query.
--
-- * 'cnqrsResponseStatus' - -- | The response status code.
createNamedQueryResponse
    :: Int -- ^ 'cnqrsResponseStatus'
    -> CreateNamedQueryResponse
createNamedQueryResponse pResponseStatus_ =
  CreateNamedQueryResponse'
    {_cnqrsNamedQueryId = Nothing, _cnqrsResponseStatus = pResponseStatus_}


-- | The unique ID of the query.
cnqrsNamedQueryId :: Lens' CreateNamedQueryResponse (Maybe Text)
cnqrsNamedQueryId = lens _cnqrsNamedQueryId (\ s a -> s{_cnqrsNamedQueryId = a})

-- | -- | The response status code.
cnqrsResponseStatus :: Lens' CreateNamedQueryResponse Int
cnqrsResponseStatus = lens _cnqrsResponseStatus (\ s a -> s{_cnqrsResponseStatus = a})

instance NFData CreateNamedQueryResponse where
