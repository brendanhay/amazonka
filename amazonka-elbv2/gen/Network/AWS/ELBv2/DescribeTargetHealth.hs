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
-- Module      : Network.AWS.ELBv2.DescribeTargetHealth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the health of the specified targets or all of your targets.
--
--
module Network.AWS.ELBv2.DescribeTargetHealth
    (
    -- * Creating a Request
      describeTargetHealth
    , DescribeTargetHealth
    -- * Request Lenses
    , dthTargets
    , dthTargetGroupARN

    -- * Destructuring the Response
    , describeTargetHealthResponse
    , DescribeTargetHealthResponse
    -- * Response Lenses
    , dthrsTargetHealthDescriptions
    , dthrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTargetHealth' smart constructor.
data DescribeTargetHealth = DescribeTargetHealth'
  { _dthTargets        :: !(Maybe [TargetDescription])
  , _dthTargetGroupARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTargetHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dthTargets' - The targets.
--
-- * 'dthTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
describeTargetHealth
    :: Text -- ^ 'dthTargetGroupARN'
    -> DescribeTargetHealth
describeTargetHealth pTargetGroupARN_ =
  DescribeTargetHealth'
    {_dthTargets = Nothing, _dthTargetGroupARN = pTargetGroupARN_}


-- | The targets.
dthTargets :: Lens' DescribeTargetHealth [TargetDescription]
dthTargets = lens _dthTargets (\ s a -> s{_dthTargets = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the target group.
dthTargetGroupARN :: Lens' DescribeTargetHealth Text
dthTargetGroupARN = lens _dthTargetGroupARN (\ s a -> s{_dthTargetGroupARN = a})

instance AWSRequest DescribeTargetHealth where
        type Rs DescribeTargetHealth =
             DescribeTargetHealthResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "DescribeTargetHealthResult"
              (\ s h x ->
                 DescribeTargetHealthResponse' <$>
                   (x .@? "TargetHealthDescriptions" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTargetHealth where

instance NFData DescribeTargetHealth where

instance ToHeaders DescribeTargetHealth where
        toHeaders = const mempty

instance ToPath DescribeTargetHealth where
        toPath = const "/"

instance ToQuery DescribeTargetHealth where
        toQuery DescribeTargetHealth'{..}
          = mconcat
              ["Action" =: ("DescribeTargetHealth" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "Targets" =:
                 toQuery (toQueryList "member" <$> _dthTargets),
               "TargetGroupArn" =: _dthTargetGroupARN]

-- | /See:/ 'describeTargetHealthResponse' smart constructor.
data DescribeTargetHealthResponse = DescribeTargetHealthResponse'
  { _dthrsTargetHealthDescriptions :: !(Maybe [TargetHealthDescription])
  , _dthrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTargetHealthResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dthrsTargetHealthDescriptions' - Information about the health of the targets.
--
-- * 'dthrsResponseStatus' - -- | The response status code.
describeTargetHealthResponse
    :: Int -- ^ 'dthrsResponseStatus'
    -> DescribeTargetHealthResponse
describeTargetHealthResponse pResponseStatus_ =
  DescribeTargetHealthResponse'
    { _dthrsTargetHealthDescriptions = Nothing
    , _dthrsResponseStatus = pResponseStatus_
    }


-- | Information about the health of the targets.
dthrsTargetHealthDescriptions :: Lens' DescribeTargetHealthResponse [TargetHealthDescription]
dthrsTargetHealthDescriptions = lens _dthrsTargetHealthDescriptions (\ s a -> s{_dthrsTargetHealthDescriptions = a}) . _Default . _Coerce

-- | -- | The response status code.
dthrsResponseStatus :: Lens' DescribeTargetHealthResponse Int
dthrsResponseStatus = lens _dthrsResponseStatus (\ s a -> s{_dthrsResponseStatus = a})

instance NFData DescribeTargetHealthResponse where
