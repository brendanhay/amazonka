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
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available types of lifecycle hooks.
--
--
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
    (
    -- * Creating a Request
      describeLifecycleHookTypes
    , DescribeLifecycleHookTypes

    -- * Destructuring the Response
    , describeLifecycleHookTypesResponse
    , DescribeLifecycleHookTypesResponse
    -- * Response Lenses
    , dlhtrsLifecycleHookTypes
    , dlhtrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes =
  DescribeLifecycleHookTypes'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLifecycleHookTypes' with the minimum fields required to make a request.
--
describeLifecycleHookTypes
    :: DescribeLifecycleHookTypes
describeLifecycleHookTypes = DescribeLifecycleHookTypes'


instance AWSRequest DescribeLifecycleHookTypes where
        type Rs DescribeLifecycleHookTypes =
             DescribeLifecycleHookTypesResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "DescribeLifecycleHookTypesResult"
              (\ s h x ->
                 DescribeLifecycleHookTypesResponse' <$>
                   (x .@? "LifecycleHookTypes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeLifecycleHookTypes where

instance NFData DescribeLifecycleHookTypes where

instance ToHeaders DescribeLifecycleHookTypes where
        toHeaders = const mempty

instance ToPath DescribeLifecycleHookTypes where
        toPath = const "/"

instance ToQuery DescribeLifecycleHookTypes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeLifecycleHookTypes" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeLifecycleHookTypesResponse' smart constructor.
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
  { _dlhtrsLifecycleHookTypes :: !(Maybe [Text])
  , _dlhtrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLifecycleHookTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlhtrsLifecycleHookTypes' - The lifecycle hook types.
--
-- * 'dlhtrsResponseStatus' - -- | The response status code.
describeLifecycleHookTypesResponse
    :: Int -- ^ 'dlhtrsResponseStatus'
    -> DescribeLifecycleHookTypesResponse
describeLifecycleHookTypesResponse pResponseStatus_ =
  DescribeLifecycleHookTypesResponse'
    { _dlhtrsLifecycleHookTypes = Nothing
    , _dlhtrsResponseStatus = pResponseStatus_
    }


-- | The lifecycle hook types.
dlhtrsLifecycleHookTypes :: Lens' DescribeLifecycleHookTypesResponse [Text]
dlhtrsLifecycleHookTypes = lens _dlhtrsLifecycleHookTypes (\ s a -> s{_dlhtrsLifecycleHookTypes = a}) . _Default . _Coerce

-- | -- | The response status code.
dlhtrsResponseStatus :: Lens' DescribeLifecycleHookTypesResponse Int
dlhtrsResponseStatus = lens _dlhtrsResponseStatus (\ s a -> s{_dlhtrsResponseStatus = a})

instance NFData DescribeLifecycleHookTypesResponse
         where
