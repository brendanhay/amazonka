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
-- Module      : Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the termination policies supported by Auto Scaling.
--
--
module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
    (
    -- * Creating a Request
      describeTerminationPolicyTypes
    , DescribeTerminationPolicyTypes

    -- * Destructuring the Response
    , describeTerminationPolicyTypesResponse
    , DescribeTerminationPolicyTypesResponse
    -- * Response Lenses
    , dtptrsTerminationPolicyTypes
    , dtptrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTerminationPolicyTypes' smart constructor.
data DescribeTerminationPolicyTypes =
  DescribeTerminationPolicyTypes'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTerminationPolicyTypes' with the minimum fields required to make a request.
--
describeTerminationPolicyTypes
    :: DescribeTerminationPolicyTypes
describeTerminationPolicyTypes = DescribeTerminationPolicyTypes'


instance AWSRequest DescribeTerminationPolicyTypes
         where
        type Rs DescribeTerminationPolicyTypes =
             DescribeTerminationPolicyTypesResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "DescribeTerminationPolicyTypesResult"
              (\ s h x ->
                 DescribeTerminationPolicyTypesResponse' <$>
                   (x .@? "TerminationPolicyTypes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTerminationPolicyTypes
         where

instance NFData DescribeTerminationPolicyTypes where

instance ToHeaders DescribeTerminationPolicyTypes
         where
        toHeaders = const mempty

instance ToPath DescribeTerminationPolicyTypes where
        toPath = const "/"

instance ToQuery DescribeTerminationPolicyTypes where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeTerminationPolicyTypes" :: ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeTerminationPolicyTypesResponse' smart constructor.
data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse'
  { _dtptrsTerminationPolicyTypes :: !(Maybe [Text])
  , _dtptrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTerminationPolicyTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtptrsTerminationPolicyTypes' - The termination policies supported by Auto Scaling (@OldestInstance@ , @OldestLaunchConfiguration@ , @NewestInstance@ , @ClosestToNextInstanceHour@ , and @Default@ ).
--
-- * 'dtptrsResponseStatus' - -- | The response status code.
describeTerminationPolicyTypesResponse
    :: Int -- ^ 'dtptrsResponseStatus'
    -> DescribeTerminationPolicyTypesResponse
describeTerminationPolicyTypesResponse pResponseStatus_ =
  DescribeTerminationPolicyTypesResponse'
    { _dtptrsTerminationPolicyTypes = Nothing
    , _dtptrsResponseStatus = pResponseStatus_
    }


-- | The termination policies supported by Auto Scaling (@OldestInstance@ , @OldestLaunchConfiguration@ , @NewestInstance@ , @ClosestToNextInstanceHour@ , and @Default@ ).
dtptrsTerminationPolicyTypes :: Lens' DescribeTerminationPolicyTypesResponse [Text]
dtptrsTerminationPolicyTypes = lens _dtptrsTerminationPolicyTypes (\ s a -> s{_dtptrsTerminationPolicyTypes = a}) . _Default . _Coerce

-- | -- | The response status code.
dtptrsResponseStatus :: Lens' DescribeTerminationPolicyTypesResponse Int
dtptrsResponseStatus = lens _dtptrsResponseStatus (\ s a -> s{_dtptrsResponseStatus = a})

instance NFData
           DescribeTerminationPolicyTypesResponse
         where
