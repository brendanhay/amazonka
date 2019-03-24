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
-- Module      : Network.AWS.CodeBuild.ListSourceCredentials
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @SourceCredentialsInfo@ objects.
--
--
module Network.AWS.CodeBuild.ListSourceCredentials
    (
    -- * Creating a Request
      listSourceCredentials
    , ListSourceCredentials

    -- * Destructuring the Response
    , listSourceCredentialsResponse
    , ListSourceCredentialsResponse
    -- * Response Lenses
    , lscrsSourceCredentialsInfos
    , lscrsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSourceCredentials' smart constructor.
data ListSourceCredentials =
  ListSourceCredentials'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSourceCredentials' with the minimum fields required to make a request.
--
listSourceCredentials
    :: ListSourceCredentials
listSourceCredentials = ListSourceCredentials'


instance AWSRequest ListSourceCredentials where
        type Rs ListSourceCredentials =
             ListSourceCredentialsResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 ListSourceCredentialsResponse' <$>
                   (x .?> "sourceCredentialsInfos" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListSourceCredentials where

instance NFData ListSourceCredentials where

instance ToHeaders ListSourceCredentials where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.ListSourceCredentials" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSourceCredentials where
        toJSON = const (Object mempty)

instance ToPath ListSourceCredentials where
        toPath = const "/"

instance ToQuery ListSourceCredentials where
        toQuery = const mempty

-- | /See:/ 'listSourceCredentialsResponse' smart constructor.
data ListSourceCredentialsResponse = ListSourceCredentialsResponse'
  { _lscrsSourceCredentialsInfos :: !(Maybe [SourceCredentialsInfo])
  , _lscrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSourceCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lscrsSourceCredentialsInfos' - A list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@ object includes the authentication type, token ARN, and type of source provider for one set of credentials.
--
-- * 'lscrsResponseStatus' - -- | The response status code.
listSourceCredentialsResponse
    :: Int -- ^ 'lscrsResponseStatus'
    -> ListSourceCredentialsResponse
listSourceCredentialsResponse pResponseStatus_ =
  ListSourceCredentialsResponse'
    { _lscrsSourceCredentialsInfos = Nothing
    , _lscrsResponseStatus = pResponseStatus_
    }


-- | A list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@ object includes the authentication type, token ARN, and type of source provider for one set of credentials.
lscrsSourceCredentialsInfos :: Lens' ListSourceCredentialsResponse [SourceCredentialsInfo]
lscrsSourceCredentialsInfos = lens _lscrsSourceCredentialsInfos (\ s a -> s{_lscrsSourceCredentialsInfos = a}) . _Default . _Coerce

-- | -- | The response status code.
lscrsResponseStatus :: Lens' ListSourceCredentialsResponse Int
lscrsResponseStatus = lens _lscrsResponseStatus (\ s a -> s{_lscrsResponseStatus = a})

instance NFData ListSourceCredentialsResponse where
