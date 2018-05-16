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
-- Module      : Network.AWS.IoT.ListRoleAliases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the role aliases registered in your account.
--
--
module Network.AWS.IoT.ListRoleAliases
    (
    -- * Creating a Request
      listRoleAliases
    , ListRoleAliases
    -- * Request Lenses
    , lraMarker
    , lraAscendingOrder
    , lraPageSize

    -- * Destructuring the Response
    , listRoleAliasesResponse
    , ListRoleAliasesResponse
    -- * Response Lenses
    , lrarsRoleAliases
    , lrarsNextMarker
    , lrarsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRoleAliases' smart constructor.
data ListRoleAliases = ListRoleAliases'
  { _lraMarker         :: !(Maybe Text)
  , _lraAscendingOrder :: !(Maybe Bool)
  , _lraPageSize       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRoleAliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lraMarker' - A marker used to get the next set of results.
--
-- * 'lraAscendingOrder' - Return the list of role aliases in ascending alphabetical order.
--
-- * 'lraPageSize' - The maximum number of results to return at one time.
listRoleAliases
    :: ListRoleAliases
listRoleAliases =
  ListRoleAliases'
    {_lraMarker = Nothing, _lraAscendingOrder = Nothing, _lraPageSize = Nothing}


-- | A marker used to get the next set of results.
lraMarker :: Lens' ListRoleAliases (Maybe Text)
lraMarker = lens _lraMarker (\ s a -> s{_lraMarker = a})

-- | Return the list of role aliases in ascending alphabetical order.
lraAscendingOrder :: Lens' ListRoleAliases (Maybe Bool)
lraAscendingOrder = lens _lraAscendingOrder (\ s a -> s{_lraAscendingOrder = a})

-- | The maximum number of results to return at one time.
lraPageSize :: Lens' ListRoleAliases (Maybe Natural)
lraPageSize = lens _lraPageSize (\ s a -> s{_lraPageSize = a}) . mapping _Nat

instance AWSRequest ListRoleAliases where
        type Rs ListRoleAliases = ListRoleAliasesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListRoleAliasesResponse' <$>
                   (x .?> "roleAliases" .!@ mempty) <*>
                     (x .?> "nextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListRoleAliases where

instance NFData ListRoleAliases where

instance ToHeaders ListRoleAliases where
        toHeaders = const mempty

instance ToPath ListRoleAliases where
        toPath = const "/role-aliases"

instance ToQuery ListRoleAliases where
        toQuery ListRoleAliases'{..}
          = mconcat
              ["marker" =: _lraMarker,
               "isAscendingOrder" =: _lraAscendingOrder,
               "pageSize" =: _lraPageSize]

-- | /See:/ 'listRoleAliasesResponse' smart constructor.
data ListRoleAliasesResponse = ListRoleAliasesResponse'
  { _lrarsRoleAliases    :: !(Maybe [Text])
  , _lrarsNextMarker     :: !(Maybe Text)
  , _lrarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRoleAliasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrarsRoleAliases' - The role aliases.
--
-- * 'lrarsNextMarker' - A marker used to get the next set of results.
--
-- * 'lrarsResponseStatus' - -- | The response status code.
listRoleAliasesResponse
    :: Int -- ^ 'lrarsResponseStatus'
    -> ListRoleAliasesResponse
listRoleAliasesResponse pResponseStatus_ =
  ListRoleAliasesResponse'
    { _lrarsRoleAliases = Nothing
    , _lrarsNextMarker = Nothing
    , _lrarsResponseStatus = pResponseStatus_
    }


-- | The role aliases.
lrarsRoleAliases :: Lens' ListRoleAliasesResponse [Text]
lrarsRoleAliases = lens _lrarsRoleAliases (\ s a -> s{_lrarsRoleAliases = a}) . _Default . _Coerce

-- | A marker used to get the next set of results.
lrarsNextMarker :: Lens' ListRoleAliasesResponse (Maybe Text)
lrarsNextMarker = lens _lrarsNextMarker (\ s a -> s{_lrarsNextMarker = a})

-- | -- | The response status code.
lrarsResponseStatus :: Lens' ListRoleAliasesResponse Int
lrarsResponseStatus = lens _lrarsResponseStatus (\ s a -> s{_lrarsResponseStatus = a})

instance NFData ListRoleAliasesResponse where
