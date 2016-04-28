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
-- Module      : Network.AWS.SES.CreateReceiptFilter
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IP address filter.
--
-- For information about setting up IP address filters, see the
-- <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide>.
--
-- This action is throttled at one request per second.
module Network.AWS.SES.CreateReceiptFilter
    (
    -- * Creating a Request
      createReceiptFilter
    , CreateReceiptFilter
    -- * Request Lenses
    , crfFilter

    -- * Destructuring the Response
    , createReceiptFilterResponse
    , CreateReceiptFilterResponse
    -- * Response Lenses
    , crfrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'createReceiptFilter' smart constructor.
newtype CreateReceiptFilter = CreateReceiptFilter'
    { _crfFilter :: ReceiptFilter
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateReceiptFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crfFilter'
createReceiptFilter
    :: ReceiptFilter -- ^ 'crfFilter'
    -> CreateReceiptFilter
createReceiptFilter pFilter_ =
    CreateReceiptFilter'
    { _crfFilter = pFilter_
    }

-- | A data structure that describes the IP address filter to create, which
-- consists of a name, an IP address range, and whether to allow or block
-- mail from it.
crfFilter :: Lens' CreateReceiptFilter ReceiptFilter
crfFilter = lens _crfFilter (\ s a -> s{_crfFilter = a});

instance AWSRequest CreateReceiptFilter where
        type Rs CreateReceiptFilter =
             CreateReceiptFilterResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "CreateReceiptFilterResult"
              (\ s h x ->
                 CreateReceiptFilterResponse' <$> (pure (fromEnum s)))

instance Hashable CreateReceiptFilter

instance NFData CreateReceiptFilter

instance ToHeaders CreateReceiptFilter where
        toHeaders = const mempty

instance ToPath CreateReceiptFilter where
        toPath = const "/"

instance ToQuery CreateReceiptFilter where
        toQuery CreateReceiptFilter'{..}
          = mconcat
              ["Action" =: ("CreateReceiptFilter" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Filter" =: _crfFilter]

-- | /See:/ 'createReceiptFilterResponse' smart constructor.
newtype CreateReceiptFilterResponse = CreateReceiptFilterResponse'
    { _crfrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateReceiptFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crfrsResponseStatus'
createReceiptFilterResponse
    :: Int -- ^ 'crfrsResponseStatus'
    -> CreateReceiptFilterResponse
createReceiptFilterResponse pResponseStatus_ =
    CreateReceiptFilterResponse'
    { _crfrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
crfrsResponseStatus :: Lens' CreateReceiptFilterResponse Int
crfrsResponseStatus = lens _crfrsResponseStatus (\ s a -> s{_crfrsResponseStatus = a});
