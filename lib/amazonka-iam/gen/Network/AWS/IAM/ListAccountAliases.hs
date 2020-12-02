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
-- Module      : Network.AWS.IAM.ListAccountAliases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account alias associated with the AWS account (Note: you can have only one). For information about using an AWS account alias, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID> in the /IAM User Guide/ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAccountAliases
    (
    -- * Creating a Request
      listAccountAliases
    , ListAccountAliases
    -- * Request Lenses
    , laaMarker
    , laaMaxItems

    -- * Destructuring the Response
    , listAccountAliasesResponse
    , ListAccountAliasesResponse
    -- * Response Lenses
    , laarsMarker
    , laarsIsTruncated
    , laarsResponseStatus
    , laarsAccountAliases
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAccountAliases' smart constructor.
data ListAccountAliases = ListAccountAliases'
  { _laaMarker   :: !(Maybe Text)
  , _laaMaxItems :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAccountAliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laaMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'laaMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
listAccountAliases
    :: ListAccountAliases
listAccountAliases =
  ListAccountAliases' {_laaMarker = Nothing, _laaMaxItems = Nothing}


-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
laaMarker :: Lens' ListAccountAliases (Maybe Text)
laaMarker = lens _laaMarker (\ s a -> s{_laaMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
laaMaxItems :: Lens' ListAccountAliases (Maybe Natural)
laaMaxItems = lens _laaMaxItems (\ s a -> s{_laaMaxItems = a}) . mapping _Nat

instance AWSPager ListAccountAliases where
        page rq rs
          | stop (rs ^. laarsIsTruncated) = Nothing
          | isNothing (rs ^. laarsMarker) = Nothing
          | otherwise =
            Just $ rq & laaMarker .~ rs ^. laarsMarker

instance AWSRequest ListAccountAliases where
        type Rs ListAccountAliases =
             ListAccountAliasesResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListAccountAliasesResult"
              (\ s h x ->
                 ListAccountAliasesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "AccountAliases" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable ListAccountAliases where

instance NFData ListAccountAliases where

instance ToHeaders ListAccountAliases where
        toHeaders = const mempty

instance ToPath ListAccountAliases where
        toPath = const "/"

instance ToQuery ListAccountAliases where
        toQuery ListAccountAliases'{..}
          = mconcat
              ["Action" =: ("ListAccountAliases" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _laaMarker, "MaxItems" =: _laaMaxItems]

-- | Contains the response to a successful 'ListAccountAliases' request.
--
--
--
-- /See:/ 'listAccountAliasesResponse' smart constructor.
data ListAccountAliasesResponse = ListAccountAliasesResponse'
  { _laarsMarker         :: !(Maybe Text)
  , _laarsIsTruncated    :: !(Maybe Bool)
  , _laarsResponseStatus :: !Int
  , _laarsAccountAliases :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAccountAliasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laarsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'laarsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'laarsResponseStatus' - -- | The response status code.
--
-- * 'laarsAccountAliases' - A list of aliases associated with the account. AWS supports only one alias per account.
listAccountAliasesResponse
    :: Int -- ^ 'laarsResponseStatus'
    -> ListAccountAliasesResponse
listAccountAliasesResponse pResponseStatus_ =
  ListAccountAliasesResponse'
    { _laarsMarker = Nothing
    , _laarsIsTruncated = Nothing
    , _laarsResponseStatus = pResponseStatus_
    , _laarsAccountAliases = mempty
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
laarsMarker :: Lens' ListAccountAliasesResponse (Maybe Text)
laarsMarker = lens _laarsMarker (\ s a -> s{_laarsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
laarsIsTruncated :: Lens' ListAccountAliasesResponse (Maybe Bool)
laarsIsTruncated = lens _laarsIsTruncated (\ s a -> s{_laarsIsTruncated = a})

-- | -- | The response status code.
laarsResponseStatus :: Lens' ListAccountAliasesResponse Int
laarsResponseStatus = lens _laarsResponseStatus (\ s a -> s{_laarsResponseStatus = a})

-- | A list of aliases associated with the account. AWS supports only one alias per account.
laarsAccountAliases :: Lens' ListAccountAliasesResponse [Text]
laarsAccountAliases = lens _laarsAccountAliases (\ s a -> s{_laarsAccountAliases = a}) . _Coerce

instance NFData ListAccountAliasesResponse where
