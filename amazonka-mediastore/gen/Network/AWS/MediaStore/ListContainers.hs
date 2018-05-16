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
-- Module      : Network.AWS.MediaStore.ListContainers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the properties of all containers in AWS Elemental MediaStore.
--
--
-- You can query to receive all the containers in one response. Or you can include the @MaxResults@ parameter to receive a limited number of containers in each response. In this case, the response includes a token. To get the next set of containers, send the command again, this time with the @NextToken@ parameter (with the returned token as its value). The next set of responses appears, with a token if there are still more containers to receive.
--
-- See also 'DescribeContainer' , which gets the properties of one container.
--
module Network.AWS.MediaStore.ListContainers
    (
    -- * Creating a Request
      listContainers
    , ListContainers
    -- * Request Lenses
    , lcNextToken
    , lcMaxResults

    -- * Destructuring the Response
    , listContainersResponse
    , ListContainersResponse
    -- * Response Lenses
    , lcrsNextToken
    , lcrsResponseStatus
    , lcrsContainers
    ) where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listContainers' smart constructor.
data ListContainers = ListContainers'
  { _lcNextToken  :: !(Maybe Text)
  , _lcMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListContainers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcNextToken' - Only if you used @MaxResults@ in the first command, enter the token (which was included in the previous response) to obtain the next set of containers. This token is included in a response only if there actually are more containers to list.
--
-- * 'lcMaxResults' - Enter the maximum number of containers in the response. Use from 1 to 255 characters.
listContainers
    :: ListContainers
listContainers =
  ListContainers' {_lcNextToken = Nothing, _lcMaxResults = Nothing}


-- | Only if you used @MaxResults@ in the first command, enter the token (which was included in the previous response) to obtain the next set of containers. This token is included in a response only if there actually are more containers to list.
lcNextToken :: Lens' ListContainers (Maybe Text)
lcNextToken = lens _lcNextToken (\ s a -> s{_lcNextToken = a})

-- | Enter the maximum number of containers in the response. Use from 1 to 255 characters.
lcMaxResults :: Lens' ListContainers (Maybe Natural)
lcMaxResults = lens _lcMaxResults (\ s a -> s{_lcMaxResults = a}) . mapping _Nat

instance AWSRequest ListContainers where
        type Rs ListContainers = ListContainersResponse
        request = postJSON mediaStore
        response
          = receiveJSON
              (\ s h x ->
                 ListContainersResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "Containers" .!@ mempty))

instance Hashable ListContainers where

instance NFData ListContainers where

instance ToHeaders ListContainers where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MediaStore_20170901.ListContainers" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListContainers where
        toJSON ListContainers'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lcNextToken,
                  ("MaxResults" .=) <$> _lcMaxResults])

instance ToPath ListContainers where
        toPath = const "/"

instance ToQuery ListContainers where
        toQuery = const mempty

-- | /See:/ 'listContainersResponse' smart constructor.
data ListContainersResponse = ListContainersResponse'
  { _lcrsNextToken      :: !(Maybe Text)
  , _lcrsResponseStatus :: !Int
  , _lcrsContainers     :: ![Container]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListContainersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsNextToken' - @NextToken@ is the token to use in the next call to @ListContainers@ . This token is returned only if you included the @MaxResults@ tag in the original command, and only if there are still containers to return.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
--
-- * 'lcrsContainers' - The names of the containers.
listContainersResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListContainersResponse
listContainersResponse pResponseStatus_ =
  ListContainersResponse'
    { _lcrsNextToken = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    , _lcrsContainers = mempty
    }


-- | @NextToken@ is the token to use in the next call to @ListContainers@ . This token is returned only if you included the @MaxResults@ tag in the original command, and only if there are still containers to return.
lcrsNextToken :: Lens' ListContainersResponse (Maybe Text)
lcrsNextToken = lens _lcrsNextToken (\ s a -> s{_lcrsNextToken = a})

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListContainersResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

-- | The names of the containers.
lcrsContainers :: Lens' ListContainersResponse [Container]
lcrsContainers = lens _lcrsContainers (\ s a -> s{_lcrsContainers = a}) . _Coerce

instance NFData ListContainersResponse where
