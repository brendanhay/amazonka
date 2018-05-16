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
-- Module      : Network.AWS.EC2.DescribeConversionTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your conversion tasks. For more information, see the <http://docs.aws.amazon.com/vm-import/latest/userguide/ VM Import/Export User Guide> .
--
--
-- For information about the import manifest referenced by this API action, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest> .
--
module Network.AWS.EC2.DescribeConversionTasks
    (
    -- * Creating a Request
      describeConversionTasks
    , DescribeConversionTasks
    -- * Request Lenses
    , dctConversionTaskIds
    , dctDryRun

    -- * Destructuring the Response
    , describeConversionTasksResponse
    , DescribeConversionTasksResponse
    -- * Response Lenses
    , dctrsConversionTasks
    , dctrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeConversionTasks.
--
--
--
-- /See:/ 'describeConversionTasks' smart constructor.
data DescribeConversionTasks = DescribeConversionTasks'
  { _dctConversionTaskIds :: !(Maybe [Text])
  , _dctDryRun            :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConversionTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dctConversionTaskIds' - One or more conversion task IDs.
--
-- * 'dctDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeConversionTasks
    :: DescribeConversionTasks
describeConversionTasks =
  DescribeConversionTasks'
    {_dctConversionTaskIds = Nothing, _dctDryRun = Nothing}


-- | One or more conversion task IDs.
dctConversionTaskIds :: Lens' DescribeConversionTasks [Text]
dctConversionTaskIds = lens _dctConversionTaskIds (\ s a -> s{_dctConversionTaskIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dctDryRun :: Lens' DescribeConversionTasks (Maybe Bool)
dctDryRun = lens _dctDryRun (\ s a -> s{_dctDryRun = a})

instance AWSRequest DescribeConversionTasks where
        type Rs DescribeConversionTasks =
             DescribeConversionTasksResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeConversionTasksResponse' <$>
                   (x .@? "conversionTasks" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConversionTasks where

instance NFData DescribeConversionTasks where

instance ToHeaders DescribeConversionTasks where
        toHeaders = const mempty

instance ToPath DescribeConversionTasks where
        toPath = const "/"

instance ToQuery DescribeConversionTasks where
        toQuery DescribeConversionTasks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeConversionTasks" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "ConversionTaskId" <$>
                    _dctConversionTaskIds),
               "DryRun" =: _dctDryRun]

-- | Contains the output for DescribeConversionTasks.
--
--
--
-- /See:/ 'describeConversionTasksResponse' smart constructor.
data DescribeConversionTasksResponse = DescribeConversionTasksResponse'
  { _dctrsConversionTasks :: !(Maybe [ConversionTask])
  , _dctrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConversionTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dctrsConversionTasks' - Information about the conversion tasks.
--
-- * 'dctrsResponseStatus' - -- | The response status code.
describeConversionTasksResponse
    :: Int -- ^ 'dctrsResponseStatus'
    -> DescribeConversionTasksResponse
describeConversionTasksResponse pResponseStatus_ =
  DescribeConversionTasksResponse'
    {_dctrsConversionTasks = Nothing, _dctrsResponseStatus = pResponseStatus_}


-- | Information about the conversion tasks.
dctrsConversionTasks :: Lens' DescribeConversionTasksResponse [ConversionTask]
dctrsConversionTasks = lens _dctrsConversionTasks (\ s a -> s{_dctrsConversionTasks = a}) . _Default . _Coerce

-- | -- | The response status code.
dctrsResponseStatus :: Lens' DescribeConversionTasksResponse Int
dctrsResponseStatus = lens _dctrsResponseStatus (\ s a -> s{_dctrsResponseStatus = a})

instance NFData DescribeConversionTasksResponse where
