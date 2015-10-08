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
-- Module      : Network.AWS.Inspector.DescribeRulesPackage
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the rules package specified by the rules package ARN.
--
-- /See:/ <http://docs.aws.amazon.com/inspector/latest/APIReference/API_DescribeRulesPackage.html AWS API Reference> for DescribeRulesPackage.
module Network.AWS.Inspector.DescribeRulesPackage
    (
    -- * Creating a Request
      describeRulesPackage
    , DescribeRulesPackage
    -- * Request Lenses
    , drpRulesPackageARN

    -- * Destructuring the Response
    , describeRulesPackageResponse
    , DescribeRulesPackageResponse
    -- * Response Lenses
    , drprsRulesPackage
    , drprsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeRulesPackage' smart constructor.
newtype DescribeRulesPackage = DescribeRulesPackage'
    { _drpRulesPackageARN :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeRulesPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpRulesPackageARN'
describeRulesPackage
    :: DescribeRulesPackage
describeRulesPackage =
    DescribeRulesPackage'
    { _drpRulesPackageARN = Nothing
    }

-- | The ARN specifying the rules package that you want to describe.
drpRulesPackageARN :: Lens' DescribeRulesPackage (Maybe Text)
drpRulesPackageARN = lens _drpRulesPackageARN (\ s a -> s{_drpRulesPackageARN = a});

instance AWSRequest DescribeRulesPackage where
        type Rs DescribeRulesPackage =
             DescribeRulesPackageResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRulesPackageResponse' <$>
                   (x .?> "rulesPackage") <*> (pure (fromEnum s)))

instance ToHeaders DescribeRulesPackage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeRulesPackage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRulesPackage where
        toJSON DescribeRulesPackage'{..}
          = object
              (catMaybes
                 [("rulesPackageArn" .=) <$> _drpRulesPackageARN])

instance ToPath DescribeRulesPackage where
        toPath = const "/"

instance ToQuery DescribeRulesPackage where
        toQuery = const mempty

-- | /See:/ 'describeRulesPackageResponse' smart constructor.
data DescribeRulesPackageResponse = DescribeRulesPackageResponse'
    { _drprsRulesPackage   :: !(Maybe RulesPackage)
    , _drprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeRulesPackageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drprsRulesPackage'
--
-- * 'drprsResponseStatus'
describeRulesPackageResponse
    :: Int -- ^ 'drprsResponseStatus'
    -> DescribeRulesPackageResponse
describeRulesPackageResponse pResponseStatus_ =
    DescribeRulesPackageResponse'
    { _drprsRulesPackage = Nothing
    , _drprsResponseStatus = pResponseStatus_
    }

-- | Information about the rules package.
drprsRulesPackage :: Lens' DescribeRulesPackageResponse (Maybe RulesPackage)
drprsRulesPackage = lens _drprsRulesPackage (\ s a -> s{_drprsRulesPackage = a});

-- | The response status code.
drprsResponseStatus :: Lens' DescribeRulesPackageResponse Int
drprsResponseStatus = lens _drprsResponseStatus (\ s a -> s{_drprsResponseStatus = a});
