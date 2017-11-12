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
-- Module      : Network.AWS.AppStream.DescribeImageBuilders
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.AppStream.DescribeImageBuilders
    (
    -- * Creating a Request
      describeImageBuilders
    , DescribeImageBuilders
    -- * Request Lenses
    , dibNextToken
    , dibNames
    , dibMaxResults

    -- * Destructuring the Response
    , describeImageBuildersResponse
    , DescribeImageBuildersResponse
    -- * Response Lenses
    , dibsrsImageBuilders
    , dibsrsNextToken
    , dibsrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImageBuilders' smart constructor.
data DescribeImageBuilders = DescribeImageBuilders'
  { _dibNextToken  :: !(Maybe Text)
  , _dibNames      :: !(Maybe [Text])
  , _dibMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImageBuilders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dibNextToken' - Undocumented member.
--
-- * 'dibNames' - Undocumented member.
--
-- * 'dibMaxResults' - Undocumented member.
describeImageBuilders
    :: DescribeImageBuilders
describeImageBuilders =
  DescribeImageBuilders'
  {_dibNextToken = Nothing, _dibNames = Nothing, _dibMaxResults = Nothing}


-- | Undocumented member.
dibNextToken :: Lens' DescribeImageBuilders (Maybe Text)
dibNextToken = lens _dibNextToken (\ s a -> s{_dibNextToken = a});

-- | Undocumented member.
dibNames :: Lens' DescribeImageBuilders [Text]
dibNames = lens _dibNames (\ s a -> s{_dibNames = a}) . _Default . _Coerce;

-- | Undocumented member.
dibMaxResults :: Lens' DescribeImageBuilders (Maybe Int)
dibMaxResults = lens _dibMaxResults (\ s a -> s{_dibMaxResults = a});

instance AWSRequest DescribeImageBuilders where
        type Rs DescribeImageBuilders =
             DescribeImageBuildersResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeImageBuildersResponse' <$>
                   (x .?> "ImageBuilders" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeImageBuilders where

instance NFData DescribeImageBuilders where

instance ToHeaders DescribeImageBuilders where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DescribeImageBuilders" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeImageBuilders where
        toJSON DescribeImageBuilders'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dibNextToken,
                  ("Names" .=) <$> _dibNames,
                  ("MaxResults" .=) <$> _dibMaxResults])

instance ToPath DescribeImageBuilders where
        toPath = const "/"

instance ToQuery DescribeImageBuilders where
        toQuery = const mempty

-- | /See:/ 'describeImageBuildersResponse' smart constructor.
data DescribeImageBuildersResponse = DescribeImageBuildersResponse'
  { _dibsrsImageBuilders  :: !(Maybe [ImageBuilder])
  , _dibsrsNextToken      :: !(Maybe Text)
  , _dibsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImageBuildersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dibsrsImageBuilders' - Undocumented member.
--
-- * 'dibsrsNextToken' - Undocumented member.
--
-- * 'dibsrsResponseStatus' - -- | The response status code.
describeImageBuildersResponse
    :: Int -- ^ 'dibsrsResponseStatus'
    -> DescribeImageBuildersResponse
describeImageBuildersResponse pResponseStatus_ =
  DescribeImageBuildersResponse'
  { _dibsrsImageBuilders = Nothing
  , _dibsrsNextToken = Nothing
  , _dibsrsResponseStatus = pResponseStatus_
  }


-- | Undocumented member.
dibsrsImageBuilders :: Lens' DescribeImageBuildersResponse [ImageBuilder]
dibsrsImageBuilders = lens _dibsrsImageBuilders (\ s a -> s{_dibsrsImageBuilders = a}) . _Default . _Coerce;

-- | Undocumented member.
dibsrsNextToken :: Lens' DescribeImageBuildersResponse (Maybe Text)
dibsrsNextToken = lens _dibsrsNextToken (\ s a -> s{_dibsrsNextToken = a});

-- | -- | The response status code.
dibsrsResponseStatus :: Lens' DescribeImageBuildersResponse Int
dibsrsResponseStatus = lens _dibsrsResponseStatus (\ s a -> s{_dibsrsResponseStatus = a});

instance NFData DescribeImageBuildersResponse where
