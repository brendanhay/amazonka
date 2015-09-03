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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account aliases associated with the account. For information
-- about using an AWS account alias, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID>
-- in the /Using IAM/ guide.
--
-- You can paginate the results using the 'MaxItems' and 'Marker'
-- parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccountAliases.html AWS API Reference> for ListAccountAliases.
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

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAccountAliases' smart constructor.
data ListAccountAliases = ListAccountAliases'
    { _laaMarker   :: !(Maybe Text)
    , _laaMaxItems :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAccountAliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laaMarker'
--
-- * 'laaMaxItems'
listAccountAliases
    :: ListAccountAliases
listAccountAliases =
    ListAccountAliases'
    { _laaMarker = Nothing
    , _laaMaxItems = Nothing
    }

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
laaMarker :: Lens' ListAccountAliases (Maybe Text)
laaMarker = lens _laaMarker (\ s a -> s{_laaMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
laaMaxItems :: Lens' ListAccountAliases (Maybe Natural)
laaMaxItems = lens _laaMaxItems (\ s a -> s{_laaMaxItems = a}) . mapping _Nat;

instance AWSPager ListAccountAliases where
        page rq rs
          | stop (rs ^. laarsMarker) = Nothing
          | stop (rs ^. laarsAccountAliases) = Nothing
          | otherwise =
            Just $ rq & laaMarker .~ rs ^. laarsMarker

instance AWSRequest ListAccountAliases where
        type Rs ListAccountAliases =
             ListAccountAliasesResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "ListAccountAliasesResult"
              (\ s h x ->
                 ListAccountAliasesResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "AccountAliases" .!@ mempty >>=
                        parseXMLList "member"))

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

-- | Contains the response to a successful ListAccountAliases request.
--
-- /See:/ 'listAccountAliasesResponse' smart constructor.
data ListAccountAliasesResponse = ListAccountAliasesResponse'
    { _laarsMarker         :: !(Maybe Text)
    , _laarsIsTruncated    :: !(Maybe Bool)
    , _laarsResponseStatus :: !Int
    , _laarsAccountAliases :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAccountAliasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laarsMarker'
--
-- * 'laarsIsTruncated'
--
-- * 'laarsResponseStatus'
--
-- * 'laarsAccountAliases'
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

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
laarsMarker :: Lens' ListAccountAliasesResponse (Maybe Text)
laarsMarker = lens _laarsMarker (\ s a -> s{_laarsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
laarsIsTruncated :: Lens' ListAccountAliasesResponse (Maybe Bool)
laarsIsTruncated = lens _laarsIsTruncated (\ s a -> s{_laarsIsTruncated = a});

-- | The response status code.
laarsResponseStatus :: Lens' ListAccountAliasesResponse Int
laarsResponseStatus = lens _laarsResponseStatus (\ s a -> s{_laarsResponseStatus = a});

-- | A list of aliases associated with the account.
laarsAccountAliases :: Lens' ListAccountAliasesResponse [Text]
laarsAccountAliases = lens _laarsAccountAliases (\ s a -> s{_laarsAccountAliases = a}) . _Coerce;
