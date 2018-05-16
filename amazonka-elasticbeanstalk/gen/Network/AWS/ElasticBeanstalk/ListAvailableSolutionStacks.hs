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
-- Module      : Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available solution stack names, with the public version first and then in reverse chronological order.
--
--
module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
    (
    -- * Creating a Request
      listAvailableSolutionStacks
    , ListAvailableSolutionStacks

    -- * Destructuring the Response
    , listAvailableSolutionStacksResponse
    , ListAvailableSolutionStacksResponse
    -- * Response Lenses
    , lassrsSolutionStacks
    , lassrsSolutionStackDetails
    , lassrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAvailableSolutionStacks' smart constructor.
data ListAvailableSolutionStacks =
  ListAvailableSolutionStacks'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAvailableSolutionStacks' with the minimum fields required to make a request.
--
listAvailableSolutionStacks
    :: ListAvailableSolutionStacks
listAvailableSolutionStacks = ListAvailableSolutionStacks'


instance AWSRequest ListAvailableSolutionStacks where
        type Rs ListAvailableSolutionStacks =
             ListAvailableSolutionStacksResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "ListAvailableSolutionStacksResult"
              (\ s h x ->
                 ListAvailableSolutionStacksResponse' <$>
                   (x .@? "SolutionStacks" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*>
                     (x .@? "SolutionStackDetails" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListAvailableSolutionStacks where

instance NFData ListAvailableSolutionStacks where

instance ToHeaders ListAvailableSolutionStacks where
        toHeaders = const mempty

instance ToPath ListAvailableSolutionStacks where
        toPath = const "/"

instance ToQuery ListAvailableSolutionStacks where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("ListAvailableSolutionStacks" :: ByteString),
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | A list of available AWS Elastic Beanstalk solution stacks.
--
--
--
-- /See:/ 'listAvailableSolutionStacksResponse' smart constructor.
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'
  { _lassrsSolutionStacks       :: !(Maybe [Text])
  , _lassrsSolutionStackDetails :: !(Maybe [SolutionStackDescription])
  , _lassrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAvailableSolutionStacksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lassrsSolutionStacks' - A list of available solution stacks.
--
-- * 'lassrsSolutionStackDetails' - A list of available solution stacks and their 'SolutionStackDescription' .
--
-- * 'lassrsResponseStatus' - -- | The response status code.
listAvailableSolutionStacksResponse
    :: Int -- ^ 'lassrsResponseStatus'
    -> ListAvailableSolutionStacksResponse
listAvailableSolutionStacksResponse pResponseStatus_ =
  ListAvailableSolutionStacksResponse'
    { _lassrsSolutionStacks = Nothing
    , _lassrsSolutionStackDetails = Nothing
    , _lassrsResponseStatus = pResponseStatus_
    }


-- | A list of available solution stacks.
lassrsSolutionStacks :: Lens' ListAvailableSolutionStacksResponse [Text]
lassrsSolutionStacks = lens _lassrsSolutionStacks (\ s a -> s{_lassrsSolutionStacks = a}) . _Default . _Coerce

-- | A list of available solution stacks and their 'SolutionStackDescription' .
lassrsSolutionStackDetails :: Lens' ListAvailableSolutionStacksResponse [SolutionStackDescription]
lassrsSolutionStackDetails = lens _lassrsSolutionStackDetails (\ s a -> s{_lassrsSolutionStackDetails = a}) . _Default . _Coerce

-- | -- | The response status code.
lassrsResponseStatus :: Lens' ListAvailableSolutionStacksResponse Int
lassrsResponseStatus = lens _lassrsResponseStatus (\ s a -> s{_lassrsResponseStatus = a})

instance NFData ListAvailableSolutionStacksResponse
         where
