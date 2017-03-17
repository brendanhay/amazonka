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
-- Module      : Network.AWS.DirectConnect.DescribeConnectionLoa
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated in favor of 'DescribeLoa' .
--
--
-- Returns the LOA-CFA for a Connection.
--
-- The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that your APN partner or service provider uses when establishing your cross connect to AWS at the colocation facility. For more information, see <http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html Requesting Cross Connects at AWS Direct Connect Locations> in the AWS Direct Connect user guide.
--
module Network.AWS.DirectConnect.DescribeConnectionLoa
    (
    -- * Creating a Request
      describeConnectionLoa
    , DescribeConnectionLoa
    -- * Request Lenses
    , dclLoaContentType
    , dclProviderName
    , dclConnectionId

    -- * Destructuring the Response
    , describeConnectionLoaResponse
    , DescribeConnectionLoaResponse
    -- * Response Lenses
    , dclrsLoa
    , dclrsResponseStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the DescribeConnectionLoa operation.
--
--
--
-- /See:/ 'describeConnectionLoa' smart constructor.
data DescribeConnectionLoa = DescribeConnectionLoa'
    { _dclLoaContentType :: !(Maybe LoaContentType)
    , _dclProviderName   :: !(Maybe Text)
    , _dclConnectionId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConnectionLoa' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dclLoaContentType' - Undocumented member.
--
-- * 'dclProviderName' - The name of the APN partner or service provider who establishes connectivity on your behalf. If you supply this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect. Default: None
--
-- * 'dclConnectionId' - Undocumented member.
describeConnectionLoa
    :: Text -- ^ 'dclConnectionId'
    -> DescribeConnectionLoa
describeConnectionLoa pConnectionId_ =
    DescribeConnectionLoa'
    { _dclLoaContentType = Nothing
    , _dclProviderName = Nothing
    , _dclConnectionId = pConnectionId_
    }

-- | Undocumented member.
dclLoaContentType :: Lens' DescribeConnectionLoa (Maybe LoaContentType)
dclLoaContentType = lens _dclLoaContentType (\ s a -> s{_dclLoaContentType = a});

-- | The name of the APN partner or service provider who establishes connectivity on your behalf. If you supply this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect. Default: None
dclProviderName :: Lens' DescribeConnectionLoa (Maybe Text)
dclProviderName = lens _dclProviderName (\ s a -> s{_dclProviderName = a});

-- | Undocumented member.
dclConnectionId :: Lens' DescribeConnectionLoa Text
dclConnectionId = lens _dclConnectionId (\ s a -> s{_dclConnectionId = a});

instance AWSRequest DescribeConnectionLoa where
        type Rs DescribeConnectionLoa =
             DescribeConnectionLoaResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConnectionLoaResponse' <$>
                   (x .?> "loa") <*> (pure (fromEnum s)))

instance Hashable DescribeConnectionLoa

instance NFData DescribeConnectionLoa

instance ToHeaders DescribeConnectionLoa where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeConnectionLoa" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConnectionLoa where
        toJSON DescribeConnectionLoa'{..}
          = object
              (catMaybes
                 [("loaContentType" .=) <$> _dclLoaContentType,
                  ("providerName" .=) <$> _dclProviderName,
                  Just ("connectionId" .= _dclConnectionId)])

instance ToPath DescribeConnectionLoa where
        toPath = const "/"

instance ToQuery DescribeConnectionLoa where
        toQuery = const mempty

-- | The response received when DescribeConnectionLoa is called.
--
--
--
-- /See:/ 'describeConnectionLoaResponse' smart constructor.
data DescribeConnectionLoaResponse = DescribeConnectionLoaResponse'
    { _dclrsLoa            :: !(Maybe Loa)
    , _dclrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConnectionLoaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dclrsLoa' - Undocumented member.
--
-- * 'dclrsResponseStatus' - -- | The response status code.
describeConnectionLoaResponse
    :: Int -- ^ 'dclrsResponseStatus'
    -> DescribeConnectionLoaResponse
describeConnectionLoaResponse pResponseStatus_ =
    DescribeConnectionLoaResponse'
    { _dclrsLoa = Nothing
    , _dclrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dclrsLoa :: Lens' DescribeConnectionLoaResponse (Maybe Loa)
dclrsLoa = lens _dclrsLoa (\ s a -> s{_dclrsLoa = a});

-- | -- | The response status code.
dclrsResponseStatus :: Lens' DescribeConnectionLoaResponse Int
dclrsResponseStatus = lens _dclrsResponseStatus (\ s a -> s{_dclrsResponseStatus = a});

instance NFData DescribeConnectionLoaResponse
