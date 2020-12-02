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
-- Module      : Network.AWS.SES.ListReceiptFilters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IP address filters associated with your AWS account.
--
--
-- For information about managing IP address filters, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html Amazon SES Developer Guide> .
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.ListReceiptFilters
    (
    -- * Creating a Request
      listReceiptFilters
    , ListReceiptFilters

    -- * Destructuring the Response
    , listReceiptFiltersResponse
    , ListReceiptFiltersResponse
    -- * Response Lenses
    , lrfrsFilters
    , lrfrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to list the IP address filters that exist under your AWS account. You use IP address filters when you receive email with Amazon SES. For more information, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
--
--
-- /See:/ 'listReceiptFilters' smart constructor.
data ListReceiptFilters =
  ListReceiptFilters'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListReceiptFilters' with the minimum fields required to make a request.
--
listReceiptFilters
    :: ListReceiptFilters
listReceiptFilters = ListReceiptFilters'


instance AWSRequest ListReceiptFilters where
        type Rs ListReceiptFilters =
             ListReceiptFiltersResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "ListReceiptFiltersResult"
              (\ s h x ->
                 ListReceiptFiltersResponse' <$>
                   (x .@? "Filters" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListReceiptFilters where

instance NFData ListReceiptFilters where

instance ToHeaders ListReceiptFilters where
        toHeaders = const mempty

instance ToPath ListReceiptFilters where
        toPath = const "/"

instance ToQuery ListReceiptFilters where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("ListReceiptFilters" :: ByteString),
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | A list of IP address filters that exist under your AWS account.
--
--
--
-- /See:/ 'listReceiptFiltersResponse' smart constructor.
data ListReceiptFiltersResponse = ListReceiptFiltersResponse'
  { _lrfrsFilters        :: !(Maybe [ReceiptFilter])
  , _lrfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListReceiptFiltersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrfrsFilters' - A list of IP address filter data structures, which each consist of a name, an IP address range, and whether to allow or block mail from it.
--
-- * 'lrfrsResponseStatus' - -- | The response status code.
listReceiptFiltersResponse
    :: Int -- ^ 'lrfrsResponseStatus'
    -> ListReceiptFiltersResponse
listReceiptFiltersResponse pResponseStatus_ =
  ListReceiptFiltersResponse'
    {_lrfrsFilters = Nothing, _lrfrsResponseStatus = pResponseStatus_}


-- | A list of IP address filter data structures, which each consist of a name, an IP address range, and whether to allow or block mail from it.
lrfrsFilters :: Lens' ListReceiptFiltersResponse [ReceiptFilter]
lrfrsFilters = lens _lrfrsFilters (\ s a -> s{_lrfrsFilters = a}) . _Default . _Coerce

-- | -- | The response status code.
lrfrsResponseStatus :: Lens' ListReceiptFiltersResponse Int
lrfrsResponseStatus = lens _lrfrsResponseStatus (\ s a -> s{_lrfrsResponseStatus = a})

instance NFData ListReceiptFiltersResponse where
