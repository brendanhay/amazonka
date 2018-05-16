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
-- Module      : Network.AWS.DMS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the AWS DMS attributes for a customer account. The attributes include AWS DMS quotas for the account, such as the number of replication instances allowed. The description for a quota includes the quota name, current usage toward that quota, and the quota's maximum value.
--
--
-- This command does not take any parameters.
--
module Network.AWS.DMS.DescribeAccountAttributes
    (
    -- * Creating a Request
      describeAccountAttributes
    , DescribeAccountAttributes

    -- * Destructuring the Response
    , describeAccountAttributesResponse
    , DescribeAccountAttributesResponse
    -- * Response Lenses
    , daarsAccountQuotas
    , daarsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeAccountAttributes' smart constructor.
data DescribeAccountAttributes =
  DescribeAccountAttributes'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAccountAttributes' with the minimum fields required to make a request.
--
describeAccountAttributes
    :: DescribeAccountAttributes
describeAccountAttributes = DescribeAccountAttributes'


instance AWSRequest DescribeAccountAttributes where
        type Rs DescribeAccountAttributes =
             DescribeAccountAttributesResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAccountAttributesResponse' <$>
                   (x .?> "AccountQuotas" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeAccountAttributes where

instance NFData DescribeAccountAttributes where

instance ToHeaders DescribeAccountAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeAccountAttributes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAccountAttributes where
        toJSON = const (Object mempty)

instance ToPath DescribeAccountAttributes where
        toPath = const "/"

instance ToQuery DescribeAccountAttributes where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { _daarsAccountQuotas  :: !(Maybe [AccountQuota])
  , _daarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAccountAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsAccountQuotas' - Account quota information.
--
-- * 'daarsResponseStatus' - -- | The response status code.
describeAccountAttributesResponse
    :: Int -- ^ 'daarsResponseStatus'
    -> DescribeAccountAttributesResponse
describeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    {_daarsAccountQuotas = Nothing, _daarsResponseStatus = pResponseStatus_}


-- | Account quota information.
daarsAccountQuotas :: Lens' DescribeAccountAttributesResponse [AccountQuota]
daarsAccountQuotas = lens _daarsAccountQuotas (\ s a -> s{_daarsAccountQuotas = a}) . _Default . _Coerce

-- | -- | The response status code.
daarsResponseStatus :: Lens' DescribeAccountAttributesResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\ s a -> s{_daarsResponseStatus = a})

instance NFData DescribeAccountAttributesResponse
         where
