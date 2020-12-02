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
-- Module      : Network.AWS.ELBv2.DescribeTargetGroupAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified target group.
--
--
module Network.AWS.ELBv2.DescribeTargetGroupAttributes
    (
    -- * Creating a Request
      describeTargetGroupAttributes
    , DescribeTargetGroupAttributes
    -- * Request Lenses
    , dtgaTargetGroupARN

    -- * Destructuring the Response
    , describeTargetGroupAttributesResponse
    , DescribeTargetGroupAttributesResponse
    -- * Response Lenses
    , dtgarsAttributes
    , dtgarsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTargetGroupAttributes' smart constructor.
newtype DescribeTargetGroupAttributes = DescribeTargetGroupAttributes'
  { _dtgaTargetGroupARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTargetGroupAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgaTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
describeTargetGroupAttributes
    :: Text -- ^ 'dtgaTargetGroupARN'
    -> DescribeTargetGroupAttributes
describeTargetGroupAttributes pTargetGroupARN_ =
  DescribeTargetGroupAttributes' {_dtgaTargetGroupARN = pTargetGroupARN_}


-- | The Amazon Resource Name (ARN) of the target group.
dtgaTargetGroupARN :: Lens' DescribeTargetGroupAttributes Text
dtgaTargetGroupARN = lens _dtgaTargetGroupARN (\ s a -> s{_dtgaTargetGroupARN = a})

instance AWSRequest DescribeTargetGroupAttributes
         where
        type Rs DescribeTargetGroupAttributes =
             DescribeTargetGroupAttributesResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper
              "DescribeTargetGroupAttributesResult"
              (\ s h x ->
                 DescribeTargetGroupAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTargetGroupAttributes where

instance NFData DescribeTargetGroupAttributes where

instance ToHeaders DescribeTargetGroupAttributes
         where
        toHeaders = const mempty

instance ToPath DescribeTargetGroupAttributes where
        toPath = const "/"

instance ToQuery DescribeTargetGroupAttributes where
        toQuery DescribeTargetGroupAttributes'{..}
          = mconcat
              ["Action" =:
                 ("DescribeTargetGroupAttributes" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "TargetGroupArn" =: _dtgaTargetGroupARN]

-- | /See:/ 'describeTargetGroupAttributesResponse' smart constructor.
data DescribeTargetGroupAttributesResponse = DescribeTargetGroupAttributesResponse'
  { _dtgarsAttributes     :: !(Maybe [TargetGroupAttribute])
  , _dtgarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTargetGroupAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgarsAttributes' - Information about the target group attributes
--
-- * 'dtgarsResponseStatus' - -- | The response status code.
describeTargetGroupAttributesResponse
    :: Int -- ^ 'dtgarsResponseStatus'
    -> DescribeTargetGroupAttributesResponse
describeTargetGroupAttributesResponse pResponseStatus_ =
  DescribeTargetGroupAttributesResponse'
    {_dtgarsAttributes = Nothing, _dtgarsResponseStatus = pResponseStatus_}


-- | Information about the target group attributes
dtgarsAttributes :: Lens' DescribeTargetGroupAttributesResponse [TargetGroupAttribute]
dtgarsAttributes = lens _dtgarsAttributes (\ s a -> s{_dtgarsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
dtgarsResponseStatus :: Lens' DescribeTargetGroupAttributesResponse Int
dtgarsResponseStatus = lens _dtgarsResponseStatus (\ s a -> s{_dtgarsResponseStatus = a})

instance NFData DescribeTargetGroupAttributesResponse
         where
