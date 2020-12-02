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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns attributes related to AWS Elastic Beanstalk that are associated with the calling AWS account.
--
--
-- The result currently has one set of attributes—resource quotas.
--
module Network.AWS.ElasticBeanstalk.DescribeAccountAttributes
    (
    -- * Creating a Request
      describeAccountAttributes
    , DescribeAccountAttributes

    -- * Destructuring the Response
    , describeAccountAttributesResponse
    , DescribeAccountAttributesResponse
    -- * Response Lenses
    , daarsResourceQuotas
    , daarsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAccountAttributes' smart constructor.
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
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "DescribeAccountAttributesResult"
              (\ s h x ->
                 DescribeAccountAttributesResponse' <$>
                   (x .@? "ResourceQuotas") <*> (pure (fromEnum s)))

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
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | /See:/ 'describeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { _daarsResourceQuotas :: !(Maybe ResourceQuotas)
  , _daarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAccountAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsResourceQuotas' - The Elastic Beanstalk resource quotas associated with the calling AWS account.
--
-- * 'daarsResponseStatus' - -- | The response status code.
describeAccountAttributesResponse
    :: Int -- ^ 'daarsResponseStatus'
    -> DescribeAccountAttributesResponse
describeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    {_daarsResourceQuotas = Nothing, _daarsResponseStatus = pResponseStatus_}


-- | The Elastic Beanstalk resource quotas associated with the calling AWS account.
daarsResourceQuotas :: Lens' DescribeAccountAttributesResponse (Maybe ResourceQuotas)
daarsResourceQuotas = lens _daarsResourceQuotas (\ s a -> s{_daarsResourceQuotas = a})

-- | -- | The response status code.
daarsResponseStatus :: Lens' DescribeAccountAttributesResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\ s a -> s{_daarsResponseStatus = a})

instance NFData DescribeAccountAttributesResponse
         where
