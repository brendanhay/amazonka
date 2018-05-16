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
-- Module      : Network.AWS.DirectoryService.ListSchemaExtensions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all schema extensions applied to a Microsoft AD Directory.
--
--
module Network.AWS.DirectoryService.ListSchemaExtensions
    (
    -- * Creating a Request
      listSchemaExtensions
    , ListSchemaExtensions
    -- * Request Lenses
    , lseNextToken
    , lseLimit
    , lseDirectoryId

    -- * Destructuring the Response
    , listSchemaExtensionsResponse
    , ListSchemaExtensionsResponse
    -- * Response Lenses
    , lsersSchemaExtensionsInfo
    , lsersNextToken
    , lsersResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSchemaExtensions' smart constructor.
data ListSchemaExtensions = ListSchemaExtensions'
  { _lseNextToken   :: !(Maybe Text)
  , _lseLimit       :: !(Maybe Nat)
  , _lseDirectoryId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSchemaExtensions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lseNextToken' - The @ListSchemaExtensions.NextToken@ value from a previous call to @ListSchemaExtensions@ . Pass null if this is the first call.
--
-- * 'lseLimit' - The maximum number of items to return.
--
-- * 'lseDirectoryId' - The identifier of the directory from which to retrieve the schema extension information.
listSchemaExtensions
    :: Text -- ^ 'lseDirectoryId'
    -> ListSchemaExtensions
listSchemaExtensions pDirectoryId_ =
  ListSchemaExtensions'
    { _lseNextToken = Nothing
    , _lseLimit = Nothing
    , _lseDirectoryId = pDirectoryId_
    }


-- | The @ListSchemaExtensions.NextToken@ value from a previous call to @ListSchemaExtensions@ . Pass null if this is the first call.
lseNextToken :: Lens' ListSchemaExtensions (Maybe Text)
lseNextToken = lens _lseNextToken (\ s a -> s{_lseNextToken = a})

-- | The maximum number of items to return.
lseLimit :: Lens' ListSchemaExtensions (Maybe Natural)
lseLimit = lens _lseLimit (\ s a -> s{_lseLimit = a}) . mapping _Nat

-- | The identifier of the directory from which to retrieve the schema extension information.
lseDirectoryId :: Lens' ListSchemaExtensions Text
lseDirectoryId = lens _lseDirectoryId (\ s a -> s{_lseDirectoryId = a})

instance AWSRequest ListSchemaExtensions where
        type Rs ListSchemaExtensions =
             ListSchemaExtensionsResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 ListSchemaExtensionsResponse' <$>
                   (x .?> "SchemaExtensionsInfo" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListSchemaExtensions where

instance NFData ListSchemaExtensions where

instance ToHeaders ListSchemaExtensions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.ListSchemaExtensions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSchemaExtensions where
        toJSON ListSchemaExtensions'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lseNextToken,
                  ("Limit" .=) <$> _lseLimit,
                  Just ("DirectoryId" .= _lseDirectoryId)])

instance ToPath ListSchemaExtensions where
        toPath = const "/"

instance ToQuery ListSchemaExtensions where
        toQuery = const mempty

-- | /See:/ 'listSchemaExtensionsResponse' smart constructor.
data ListSchemaExtensionsResponse = ListSchemaExtensionsResponse'
  { _lsersSchemaExtensionsInfo :: !(Maybe [SchemaExtensionInfo])
  , _lsersNextToken            :: !(Maybe Text)
  , _lsersResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSchemaExtensionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsersSchemaExtensionsInfo' - Information about the schema extensions applied to the directory.
--
-- * 'lsersNextToken' - If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to retrieve the next set of items.
--
-- * 'lsersResponseStatus' - -- | The response status code.
listSchemaExtensionsResponse
    :: Int -- ^ 'lsersResponseStatus'
    -> ListSchemaExtensionsResponse
listSchemaExtensionsResponse pResponseStatus_ =
  ListSchemaExtensionsResponse'
    { _lsersSchemaExtensionsInfo = Nothing
    , _lsersNextToken = Nothing
    , _lsersResponseStatus = pResponseStatus_
    }


-- | Information about the schema extensions applied to the directory.
lsersSchemaExtensionsInfo :: Lens' ListSchemaExtensionsResponse [SchemaExtensionInfo]
lsersSchemaExtensionsInfo = lens _lsersSchemaExtensionsInfo (\ s a -> s{_lsersSchemaExtensionsInfo = a}) . _Default . _Coerce

-- | If not null, more results are available. Pass this value for the @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to retrieve the next set of items.
lsersNextToken :: Lens' ListSchemaExtensionsResponse (Maybe Text)
lsersNextToken = lens _lsersNextToken (\ s a -> s{_lsersNextToken = a})

-- | -- | The response status code.
lsersResponseStatus :: Lens' ListSchemaExtensionsResponse Int
lsersResponseStatus = lens _lsersResponseStatus (\ s a -> s{_lsersResponseStatus = a})

instance NFData ListSchemaExtensionsResponse where
