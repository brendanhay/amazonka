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
-- Module      : Network.AWS.ServiceCatalog.DescribeTagOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified TagOption.
--
--
module Network.AWS.ServiceCatalog.DescribeTagOption
    (
    -- * Creating a Request
      describeTagOption
    , DescribeTagOption
    -- * Request Lenses
    , dtoId

    -- * Destructuring the Response
    , describeTagOptionResponse
    , DescribeTagOptionResponse
    -- * Response Lenses
    , dtorsTagOptionDetail
    , dtorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeTagOption' smart constructor.
newtype DescribeTagOption = DescribeTagOption'
  { _dtoId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTagOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtoId' - The TagOption identifier.
describeTagOption
    :: Text -- ^ 'dtoId'
    -> DescribeTagOption
describeTagOption pId_ = DescribeTagOption' {_dtoId = pId_}


-- | The TagOption identifier.
dtoId :: Lens' DescribeTagOption Text
dtoId = lens _dtoId (\ s a -> s{_dtoId = a})

instance AWSRequest DescribeTagOption where
        type Rs DescribeTagOption = DescribeTagOptionResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTagOptionResponse' <$>
                   (x .?> "TagOptionDetail") <*> (pure (fromEnum s)))

instance Hashable DescribeTagOption where

instance NFData DescribeTagOption where

instance ToHeaders DescribeTagOption where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeTagOption" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTagOption where
        toJSON DescribeTagOption'{..}
          = object (catMaybes [Just ("Id" .= _dtoId)])

instance ToPath DescribeTagOption where
        toPath = const "/"

instance ToQuery DescribeTagOption where
        toQuery = const mempty

-- | /See:/ 'describeTagOptionResponse' smart constructor.
data DescribeTagOptionResponse = DescribeTagOptionResponse'
  { _dtorsTagOptionDetail :: !(Maybe TagOptionDetail)
  , _dtorsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTagOptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtorsTagOptionDetail' - Information about the TagOption.
--
-- * 'dtorsResponseStatus' - -- | The response status code.
describeTagOptionResponse
    :: Int -- ^ 'dtorsResponseStatus'
    -> DescribeTagOptionResponse
describeTagOptionResponse pResponseStatus_ =
  DescribeTagOptionResponse'
    {_dtorsTagOptionDetail = Nothing, _dtorsResponseStatus = pResponseStatus_}


-- | Information about the TagOption.
dtorsTagOptionDetail :: Lens' DescribeTagOptionResponse (Maybe TagOptionDetail)
dtorsTagOptionDetail = lens _dtorsTagOptionDetail (\ s a -> s{_dtorsTagOptionDetail = a})

-- | -- | The response status code.
dtorsResponseStatus :: Lens' DescribeTagOptionResponse Int
dtorsResponseStatus = lens _dtorsResponseStatus (\ s a -> s{_dtorsResponseStatus = a})

instance NFData DescribeTagOptionResponse where
