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
-- Module      : Network.AWS.Inspector.DescribeFinding
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the finding specified by the finding ARN.
module Network.AWS.Inspector.DescribeFinding
    (
    -- * Creating a Request
      describeFinding
    , DescribeFinding
    -- * Request Lenses
    , dfFindingARN

    -- * Destructuring the Response
    , describeFindingResponse
    , DescribeFindingResponse
    -- * Response Lenses
    , dfrsFinding
    , dfrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeFinding' smart constructor.
newtype DescribeFinding = DescribeFinding'
    { _dfFindingARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeFinding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfFindingARN'
describeFinding
    :: Text -- ^ 'dfFindingARN'
    -> DescribeFinding
describeFinding pFindingARN_ =
    DescribeFinding'
    { _dfFindingARN = pFindingARN_
    }

-- | The ARN specifying the finding that you want to describe.
dfFindingARN :: Lens' DescribeFinding Text
dfFindingARN = lens _dfFindingARN (\ s a -> s{_dfFindingARN = a});

instance AWSRequest DescribeFinding where
        type Rs DescribeFinding = DescribeFindingResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeFindingResponse' <$>
                   (x .?> "finding") <*> (pure (fromEnum s)))

instance Hashable DescribeFinding

instance ToHeaders DescribeFinding where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeFinding" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeFinding where
        toJSON DescribeFinding'{..}
          = object
              (catMaybes [Just ("findingArn" .= _dfFindingARN)])

instance ToPath DescribeFinding where
        toPath = const "/"

instance ToQuery DescribeFinding where
        toQuery = const mempty

-- | /See:/ 'describeFindingResponse' smart constructor.
data DescribeFindingResponse = DescribeFindingResponse'
    { _dfrsFinding        :: !(Maybe Finding)
    , _dfrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeFindingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsFinding'
--
-- * 'dfrsResponseStatus'
describeFindingResponse
    :: Int -- ^ 'dfrsResponseStatus'
    -> DescribeFindingResponse
describeFindingResponse pResponseStatus_ =
    DescribeFindingResponse'
    { _dfrsFinding = Nothing
    , _dfrsResponseStatus = pResponseStatus_
    }

-- | Information about the finding.
dfrsFinding :: Lens' DescribeFindingResponse (Maybe Finding)
dfrsFinding = lens _dfrsFinding (\ s a -> s{_dfrsFinding = a});

-- | The response status code.
dfrsResponseStatus :: Lens' DescribeFindingResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\ s a -> s{_dfrsResponseStatus = a});
