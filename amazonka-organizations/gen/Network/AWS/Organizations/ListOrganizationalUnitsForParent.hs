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
-- Module      : Network.AWS.Organizations.ListOrganizationalUnitsForParent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the organizational units (OUs) in a parent organizational unit or root.
--
--
-- This operation can be called only from the organization's master account.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListOrganizationalUnitsForParent
    (
    -- * Creating a Request
      listOrganizationalUnitsForParent
    , ListOrganizationalUnitsForParent
    -- * Request Lenses
    , loufpNextToken
    , loufpMaxResults
    , loufpParentId

    -- * Destructuring the Response
    , listOrganizationalUnitsForParentResponse
    , ListOrganizationalUnitsForParentResponse
    -- * Response Lenses
    , loufprsNextToken
    , loufprsOrganizationalUnits
    , loufprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listOrganizationalUnitsForParent' smart constructor.
data ListOrganizationalUnitsForParent = ListOrganizationalUnitsForParent'
  { _loufpNextToken  :: !(Maybe Text)
  , _loufpMaxResults :: !(Maybe Nat)
  , _loufpParentId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOrganizationalUnitsForParent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loufpNextToken' - Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'loufpMaxResults' - (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- * 'loufpParentId' - The unique identifier (ID) of the root or OU whose child OUs you want to list. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
listOrganizationalUnitsForParent
    :: Text -- ^ 'loufpParentId'
    -> ListOrganizationalUnitsForParent
listOrganizationalUnitsForParent pParentId_ =
  ListOrganizationalUnitsForParent'
    { _loufpNextToken = Nothing
    , _loufpMaxResults = Nothing
    , _loufpParentId = pParentId_
    }


-- | Use this parameter if you receive a @NextToken@ response in a previous request that indicates that there is more output available. Set it to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
loufpNextToken :: Lens' ListOrganizationalUnitsForParent (Maybe Text)
loufpNextToken = lens _loufpNextToken (\ s a -> s{_loufpNextToken = a})

-- | (Optional) Use this to limit the number of results you want included in the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
loufpMaxResults :: Lens' ListOrganizationalUnitsForParent (Maybe Natural)
loufpMaxResults = lens _loufpMaxResults (\ s a -> s{_loufpMaxResults = a}) . mapping _Nat

-- | The unique identifier (ID) of the root or OU whose child OUs you want to list. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
loufpParentId :: Lens' ListOrganizationalUnitsForParent Text
loufpParentId = lens _loufpParentId (\ s a -> s{_loufpParentId = a})

instance AWSPager ListOrganizationalUnitsForParent
         where
        page rq rs
          | stop (rs ^. loufprsNextToken) = Nothing
          | stop (rs ^. loufprsOrganizationalUnits) = Nothing
          | otherwise =
            Just $ rq & loufpNextToken .~ rs ^. loufprsNextToken

instance AWSRequest ListOrganizationalUnitsForParent
         where
        type Rs ListOrganizationalUnitsForParent =
             ListOrganizationalUnitsForParentResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListOrganizationalUnitsForParentResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "OrganizationalUnits" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListOrganizationalUnitsForParent
         where

instance NFData ListOrganizationalUnitsForParent
         where

instance ToHeaders ListOrganizationalUnitsForParent
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListOrganizationalUnitsForParent"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListOrganizationalUnitsForParent
         where
        toJSON ListOrganizationalUnitsForParent'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _loufpNextToken,
                  ("MaxResults" .=) <$> _loufpMaxResults,
                  Just ("ParentId" .= _loufpParentId)])

instance ToPath ListOrganizationalUnitsForParent
         where
        toPath = const "/"

instance ToQuery ListOrganizationalUnitsForParent
         where
        toQuery = const mempty

-- | /See:/ 'listOrganizationalUnitsForParentResponse' smart constructor.
data ListOrganizationalUnitsForParentResponse = ListOrganizationalUnitsForParentResponse'
  { _loufprsNextToken           :: !(Maybe Text)
  , _loufprsOrganizationalUnits :: !(Maybe [OrganizationalUnit])
  , _loufprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListOrganizationalUnitsForParentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loufprsNextToken' - If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'loufprsOrganizationalUnits' - A list of the OUs in the specified root or parent OU.
--
-- * 'loufprsResponseStatus' - -- | The response status code.
listOrganizationalUnitsForParentResponse
    :: Int -- ^ 'loufprsResponseStatus'
    -> ListOrganizationalUnitsForParentResponse
listOrganizationalUnitsForParentResponse pResponseStatus_ =
  ListOrganizationalUnitsForParentResponse'
    { _loufprsNextToken = Nothing
    , _loufprsOrganizationalUnits = Nothing
    , _loufprsResponseStatus = pResponseStatus_
    }


-- | If present, this value indicates that there is more output available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
loufprsNextToken :: Lens' ListOrganizationalUnitsForParentResponse (Maybe Text)
loufprsNextToken = lens _loufprsNextToken (\ s a -> s{_loufprsNextToken = a})

-- | A list of the OUs in the specified root or parent OU.
loufprsOrganizationalUnits :: Lens' ListOrganizationalUnitsForParentResponse [OrganizationalUnit]
loufprsOrganizationalUnits = lens _loufprsOrganizationalUnits (\ s a -> s{_loufprsOrganizationalUnits = a}) . _Default . _Coerce

-- | -- | The response status code.
loufprsResponseStatus :: Lens' ListOrganizationalUnitsForParentResponse Int
loufprsResponseStatus = lens _loufprsResponseStatus (\ s a -> s{_loufprsResponseStatus = a})

instance NFData
           ListOrganizationalUnitsForParentResponse
         where
