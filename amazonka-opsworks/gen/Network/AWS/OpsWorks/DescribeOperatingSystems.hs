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
-- Module      : Network.AWS.OpsWorks.DescribeOperatingSystems
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the operating systems that are supported by AWS OpsWorks Stacks.
--
--
module Network.AWS.OpsWorks.DescribeOperatingSystems
    (
    -- * Creating a Request
      describeOperatingSystems
    , DescribeOperatingSystems

    -- * Destructuring the Response
    , describeOperatingSystemsResponse
    , DescribeOperatingSystemsResponse
    -- * Response Lenses
    , dosrsOperatingSystems
    , dosrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeOperatingSystems' smart constructor.
data DescribeOperatingSystems =
  DescribeOperatingSystems'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOperatingSystems' with the minimum fields required to make a request.
--
describeOperatingSystems
    :: DescribeOperatingSystems
describeOperatingSystems = DescribeOperatingSystems'


instance AWSRequest DescribeOperatingSystems where
        type Rs DescribeOperatingSystems =
             DescribeOperatingSystemsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeOperatingSystemsResponse' <$>
                   (x .?> "OperatingSystems" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeOperatingSystems where

instance NFData DescribeOperatingSystems where

instance ToHeaders DescribeOperatingSystems where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeOperatingSystems" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeOperatingSystems where
        toJSON = const (Object mempty)

instance ToPath DescribeOperatingSystems where
        toPath = const "/"

instance ToQuery DescribeOperatingSystems where
        toQuery = const mempty

-- | The response to a @DescribeOperatingSystems@ request.
--
--
--
-- /See:/ 'describeOperatingSystemsResponse' smart constructor.
data DescribeOperatingSystemsResponse = DescribeOperatingSystemsResponse'
  { _dosrsOperatingSystems :: !(Maybe [OperatingSystem])
  , _dosrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOperatingSystemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dosrsOperatingSystems' - Undocumented member.
--
-- * 'dosrsResponseStatus' - -- | The response status code.
describeOperatingSystemsResponse
    :: Int -- ^ 'dosrsResponseStatus'
    -> DescribeOperatingSystemsResponse
describeOperatingSystemsResponse pResponseStatus_ =
  DescribeOperatingSystemsResponse'
    {_dosrsOperatingSystems = Nothing, _dosrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
dosrsOperatingSystems :: Lens' DescribeOperatingSystemsResponse [OperatingSystem]
dosrsOperatingSystems = lens _dosrsOperatingSystems (\ s a -> s{_dosrsOperatingSystems = a}) . _Default . _Coerce

-- | -- | The response status code.
dosrsResponseStatus :: Lens' DescribeOperatingSystemsResponse Int
dosrsResponseStatus = lens _dosrsResponseStatus (\ s a -> s{_dosrsResponseStatus = a})

instance NFData DescribeOperatingSystemsResponse
         where
