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
-- Module      : Network.AWS.RDS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the attributes for a customer account. The attributes include Amazon RDS quotas for the account, such as the number of DB instances allowed. The description for a quota includes the quota name, current usage toward that quota, and the quota's maximum value.
--
--
-- This command doesn't take any parameters.
--
module Network.AWS.RDS.DescribeAccountAttributes
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
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
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeAccountAttributesResult"
              (\ s h x ->
                 DescribeAccountAttributesResponse' <$>
                   (x .@? "AccountQuotas" .!@ mempty >>=
                      may (parseXMLList "AccountQuota"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAccountAttributes where

instance NFData DescribeAccountAttributes where

instance ToHeaders DescribeAccountAttributes where
        toHeaders = const mempty

instance ToPath DescribeAccountAttributes where
        toPath = const "/"

instance ToQuery DescribeAccountAttributes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeAccountAttributes" :: ByteString),
                  "Version" =: ("2014-10-31" :: ByteString)])

-- | Data returned by the __DescribeAccountAttributes__ action.
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
-- * 'daarsAccountQuotas' - A list of 'AccountQuota' objects. Within this list, each quota has a name, a count of usage toward the quota maximum, and a maximum value for the quota.
--
-- * 'daarsResponseStatus' - -- | The response status code.
describeAccountAttributesResponse
    :: Int -- ^ 'daarsResponseStatus'
    -> DescribeAccountAttributesResponse
describeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    {_daarsAccountQuotas = Nothing, _daarsResponseStatus = pResponseStatus_}


-- | A list of 'AccountQuota' objects. Within this list, each quota has a name, a count of usage toward the quota maximum, and a maximum value for the quota.
daarsAccountQuotas :: Lens' DescribeAccountAttributesResponse [AccountQuota]
daarsAccountQuotas = lens _daarsAccountQuotas (\ s a -> s{_daarsAccountQuotas = a}) . _Default . _Coerce

-- | -- | The response status code.
daarsResponseStatus :: Lens' DescribeAccountAttributesResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\ s a -> s{_daarsResponseStatus = a})

instance NFData DescribeAccountAttributesResponse
         where
