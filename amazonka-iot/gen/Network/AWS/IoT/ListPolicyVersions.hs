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
-- Module      : Network.AWS.IoT.ListPolicyVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of the specified policy and identifies the default version.
--
--
module Network.AWS.IoT.ListPolicyVersions
    (
    -- * Creating a Request
      listPolicyVersions
    , ListPolicyVersions
    -- * Request Lenses
    , lpvPolicyName

    -- * Destructuring the Response
    , listPolicyVersionsResponse
    , ListPolicyVersionsResponse
    -- * Response Lenses
    , lpvrsPolicyVersions
    , lpvrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the ListPolicyVersions operation.
--
--
--
-- /See:/ 'listPolicyVersions' smart constructor.
newtype ListPolicyVersions = ListPolicyVersions'
  { _lpvPolicyName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicyVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvPolicyName' - The policy name.
listPolicyVersions
    :: Text -- ^ 'lpvPolicyName'
    -> ListPolicyVersions
listPolicyVersions pPolicyName_ =
  ListPolicyVersions' {_lpvPolicyName = pPolicyName_}


-- | The policy name.
lpvPolicyName :: Lens' ListPolicyVersions Text
lpvPolicyName = lens _lpvPolicyName (\ s a -> s{_lpvPolicyName = a})

instance AWSRequest ListPolicyVersions where
        type Rs ListPolicyVersions =
             ListPolicyVersionsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListPolicyVersionsResponse' <$>
                   (x .?> "policyVersions" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListPolicyVersions where

instance NFData ListPolicyVersions where

instance ToHeaders ListPolicyVersions where
        toHeaders = const mempty

instance ToPath ListPolicyVersions where
        toPath ListPolicyVersions'{..}
          = mconcat
              ["/policies/", toBS _lpvPolicyName, "/version"]

instance ToQuery ListPolicyVersions where
        toQuery = const mempty

-- | The output from the ListPolicyVersions operation.
--
--
--
-- /See:/ 'listPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
  { _lpvrsPolicyVersions :: !(Maybe [PolicyVersion])
  , _lpvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicyVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvrsPolicyVersions' - The policy versions.
--
-- * 'lpvrsResponseStatus' - -- | The response status code.
listPolicyVersionsResponse
    :: Int -- ^ 'lpvrsResponseStatus'
    -> ListPolicyVersionsResponse
listPolicyVersionsResponse pResponseStatus_ =
  ListPolicyVersionsResponse'
    {_lpvrsPolicyVersions = Nothing, _lpvrsResponseStatus = pResponseStatus_}


-- | The policy versions.
lpvrsPolicyVersions :: Lens' ListPolicyVersionsResponse [PolicyVersion]
lpvrsPolicyVersions = lens _lpvrsPolicyVersions (\ s a -> s{_lpvrsPolicyVersions = a}) . _Default . _Coerce

-- | -- | The response status code.
lpvrsResponseStatus :: Lens' ListPolicyVersionsResponse Int
lpvrsResponseStatus = lens _lpvrsResponseStatus (\ s a -> s{_lpvrsResponseStatus = a})

instance NFData ListPolicyVersionsResponse where
