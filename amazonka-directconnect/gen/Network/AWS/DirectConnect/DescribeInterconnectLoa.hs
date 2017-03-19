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
-- Module      : Network.AWS.DirectConnect.DescribeInterconnectLoa
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated in favor of 'DescribeLoa' .
--
--
-- Returns the LOA-CFA for an Interconnect.
--
-- The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html Requesting Cross Connects at AWS Direct Connect Locations> in the AWS Direct Connect user guide.
--
module Network.AWS.DirectConnect.DescribeInterconnectLoa
    (
    -- * Creating a Request
      describeInterconnectLoa
    , DescribeInterconnectLoa
    -- * Request Lenses
    , dilLoaContentType
    , dilProviderName
    , dilInterconnectId

    -- * Destructuring the Response
    , describeInterconnectLoaResponse
    , DescribeInterconnectLoaResponse
    -- * Response Lenses
    , dilrsLoa
    , dilrsResponseStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DescribeInterconnectLoa operation.
--
--
--
-- /See:/ 'describeInterconnectLoa' smart constructor.
data DescribeInterconnectLoa = DescribeInterconnectLoa'
    { _dilLoaContentType :: !(Maybe LoaContentType)
    , _dilProviderName   :: !(Maybe Text)
    , _dilInterconnectId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeInterconnectLoa' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dilLoaContentType' - Undocumented member.
--
-- * 'dilProviderName' - The name of the service provider who establishes connectivity on your behalf. If you supply this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect. Default: None
--
-- * 'dilInterconnectId' - Undocumented member.
describeInterconnectLoa
    :: Text -- ^ 'dilInterconnectId'
    -> DescribeInterconnectLoa
describeInterconnectLoa pInterconnectId_ =
    DescribeInterconnectLoa'
    { _dilLoaContentType = Nothing
    , _dilProviderName = Nothing
    , _dilInterconnectId = pInterconnectId_
    }

-- | Undocumented member.
dilLoaContentType :: Lens' DescribeInterconnectLoa (Maybe LoaContentType)
dilLoaContentType = lens _dilLoaContentType (\ s a -> s{_dilLoaContentType = a});

-- | The name of the service provider who establishes connectivity on your behalf. If you supply this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect. Default: None
dilProviderName :: Lens' DescribeInterconnectLoa (Maybe Text)
dilProviderName = lens _dilProviderName (\ s a -> s{_dilProviderName = a});

-- | Undocumented member.
dilInterconnectId :: Lens' DescribeInterconnectLoa Text
dilInterconnectId = lens _dilInterconnectId (\ s a -> s{_dilInterconnectId = a});

instance AWSRequest DescribeInterconnectLoa where
        type Rs DescribeInterconnectLoa =
             DescribeInterconnectLoaResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInterconnectLoaResponse' <$>
                   (x .?> "loa") <*> (pure (fromEnum s)))

instance Hashable DescribeInterconnectLoa

instance NFData DescribeInterconnectLoa

instance ToHeaders DescribeInterconnectLoa where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeInterconnectLoa" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeInterconnectLoa where
        toJSON DescribeInterconnectLoa'{..}
          = object
              (catMaybes
                 [("loaContentType" .=) <$> _dilLoaContentType,
                  ("providerName" .=) <$> _dilProviderName,
                  Just ("interconnectId" .= _dilInterconnectId)])

instance ToPath DescribeInterconnectLoa where
        toPath = const "/"

instance ToQuery DescribeInterconnectLoa where
        toQuery = const mempty

-- | The response received when DescribeInterconnectLoa is called.
--
--
--
-- /See:/ 'describeInterconnectLoaResponse' smart constructor.
data DescribeInterconnectLoaResponse = DescribeInterconnectLoaResponse'
    { _dilrsLoa            :: !(Maybe Loa)
    , _dilrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeInterconnectLoaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dilrsLoa' - Undocumented member.
--
-- * 'dilrsResponseStatus' - -- | The response status code.
describeInterconnectLoaResponse
    :: Int -- ^ 'dilrsResponseStatus'
    -> DescribeInterconnectLoaResponse
describeInterconnectLoaResponse pResponseStatus_ =
    DescribeInterconnectLoaResponse'
    { _dilrsLoa = Nothing
    , _dilrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dilrsLoa :: Lens' DescribeInterconnectLoaResponse (Maybe Loa)
dilrsLoa = lens _dilrsLoa (\ s a -> s{_dilrsLoa = a});

-- | -- | The response status code.
dilrsResponseStatus :: Lens' DescribeInterconnectLoaResponse Int
dilrsResponseStatus = lens _dilrsResponseStatus (\ s a -> s{_dilrsResponseStatus = a});

instance NFData DescribeInterconnectLoaResponse
